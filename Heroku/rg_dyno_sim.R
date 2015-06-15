# you can make a text file of request times (in ms, one number per line) and import it here, or you can use a probability distribution to simulate request times (see below where setting req_durations_in_ms)
# rq = read.table("~/Downloads/request_times.txt", header=FALSE)$V1

# argument notes:
# parallel_router_count is only relevant if router_mode is set to "intelligent"
# choice_of_two, power_of_two, and unicorn_workers_per_dyno are only relevant if router_mode is set to "naive"
# you can only select one of choice_of_two, power_of_two, and unicorn_workers_per_dyno

run_simulation = function(router_mode = "naive",
                          reqs_per_minute = 9000,
                          simulation_length_in_minutes = 5,
                          dyno_count = 100,
                          choice_of_two = FALSE,
                          power_of_two = FALSE,
                          unicorn_workers_per_dyno = 0,
                          track_dyno_queues = FALSE,
                          parallel_router_count = 1) {
  
  if(!(router_mode %in% c("naive", "intelligent"))) {
    return("router_mode must be one of 'naive' or 'intelligent'")
  }
  
  unicorn = as.numeric(unicorn_workers_per_dyno) > 0
  
  if(sum(c(choice_of_two, power_of_two, unicorn)) > 1) {
    return("you can only set one of choice_of_two, power_of_two, and unicorn!")
  }
  
  reqs_per_ms = reqs_per_minute / 60000
  simulation_length_in_ms = ceiling(simulation_length_in_minutes * 60000)
  
  # reqs_starting is a vector where reqs_starting[i] represents the number of requests that start at millisecond i
  reqs_starting = rpois(simulation_length_in_ms, reqs_per_ms)
  
  # total number of requests for duration of simulation
  total_requests = sum(reqs_starting)
  
  # req_durations_in_ms[i] represents the amount of time, in milliseconds, that request i will take to finish after a dyno starts working on it
  # req_durations_in_ms = sample(rq, total_requests, TRUE)
  req_durations_in_ms = ceiling(rweibull(n = total_requests, shape = 0.8, scale = 79.056))
  
  # For our simulation we used an empirical distribution of request times observed from our server, but we also
  # found that the Weibull distribution with shape parameter = 0.46 is a reasonable approximation.
  # You can change the code below to use whatever distribution of request times you'd like
  # wshape = 0.46
  # wlambda = 50 / (log(2) ^ (1 / wshape))
  # req_durations_in_ms = pmin(30000, pmax(10, ceiling(rweibull(nreqs, wshape, wlambda))))
  
  # the code below sets up the results matrix, which has one row for each request and will eventually have 4 columns of data:
  # 1) request start time
  # 2) request duration
  # 3) to which dyno was the request assigned?
  # 4) how much time, if any, did the request spend in queue between when the request arrived and when a dyno started working on it?
  # we can fill columns 1 and 2 based on results from above, and if we're in "naive" mode we can additionally fill column 3
  # the rest of the code will be calculate the values for column 4
  
  uniq_start_times = which(reqs_starting > 0)
  start_times = unlist(sapply(uniq_start_times, function(x) rep(x, reqs_starting[x])))
  
  if(router_mode == "naive") {
    dyno_assignments = sample(1:dyno_count, total_requests, replace=TRUE)
    router_assignments = rep(NA, total_requests)
  } else {
    dyno_assignments = rep(NA, total_requests)
    router_assignments = sample(1:parallel_router_count, total_requests, replace=TRUE)
  }
  
  results = matrix(c(start_times,
                    req_durations_in_ms,
                    dyno_assignments,
                    rep(0, total_requests),
                    rep(0, total_requests),
                    router_assignments),
                  nrow = total_requests,
                  ncol = 6,
                  dimnames = list(1:total_requests, c("start_time", "request_duration", "dyno", "time_in_queue", "end_time", "router")))
  
  # dyno_next_available[i] represents the next millisecond at which dyno i will be free to being working on a new request
  # for example, if dyno 1 gets a request at time = 100 ms and the request lasts 55 ms, then dyno_next_available[1] will be set to 155
  dyno_next_available = rep(0, dyno_count)
  
  # have to track dyno queues if you want to do power of two
  # also might want to track dyno queues if, for example, you want to plot or animate the simulation
  if(power_of_two) track_dyno_queues = TRUE
  
  if(track_dyno_queues) {
    # dynomat[i,j] represents the number of requests assigned to dyno i at time j
    dynomat = matrix(0, nrow = dyno_count, ncol = simulation_length_in_ms)
  }
  
  if(router_mode == "naive" & unicorn) {
    dyno_next_available = rep(dyno_next_available, unicorn_workers_per_dyno)
  }
  
  if(router_mode == "intelligent") {
    # routermat[i,j] represents when router i thinks dyno j will next be available
    routermat = matrix(0, nrow = parallel_router_count, ncol = dyno_count)
  }
  
  for(i in 1:nrow(results)) {
    row = results[i,]
    st = row["start_time"]
    duration = row["request_duration"]
    
    if(router_mode == "naive") {
      dyno = row["dyno"]
      
      # if using the choice of two approach, check if the random dyno is busy, and if it is then pick another random dyno
      if(choice_of_two & dyno_next_available[dyno] > st) {
        dyno = sample(1:dyno_count, 1)
        results[i, "dyno"] = dyno
      }
      
      # if using power of two and the first dyno is busy, poll a second dyno and pick the one that has a shorter queue depth
      if(power_of_two & dyno_next_available[dyno] > st) {
        other_dyno = sample(1:dyno_count, 1)
        
        dyno_queue_depth = dynomat[dyno, st]
        other_dyno_queue_depth = dynomat[other_dyno, st]
        
        if(other_dyno_queue_depth < dyno_queue_depth) {
          dyno = other_dyno
          results[i, "dyno"] = dyno
        }
      }
      
      # if using unicorn, pick a dyno at random, but then assign the request to a worker on that dyno based on which worker first comes available
      if(unicorn) {
        sub_dynos = seq((dyno - 1) * unicorn_workers_per_dyno + 1, length = unicorn_workers_per_dyno)
        dyno = as.numeric(sub_dynos[which(dyno_next_available[sub_dynos] <= st)[1]])
      }
    }
    else {
      # if we're in 'intelligent' mode, assign task to the first dyno that the router thinks is available (i.e. not working on some other request)
      router = row["router"]
      
      # important to pick randomly here instead of taking the lowest numbered dyno that is available
      # if we pick the first element then every router will start with dyno 1, causing unnecessary queuing
      dynos_to_choose_from = which(routermat[router,] <= st)
      dyno = ifelse(length(dynos_to_choose_from) > 0, sample(dynos_to_choose_from, 1), NA)
    }
    
    # if we've assigned a dyno and that dyno is available, then the request is not queued, and the dyno is tied up until the request is finished
    if(!is.na(dyno) && dyno_next_available[dyno] <= st) {
      dyno_next_available[dyno] = st + duration
      results[i, "end_time"] = st + duration - 1
      
      if(router_mode == "intelligent") {
        routermat[router, dyno] = st + duration
      }
      
      if(track_dyno_queues) {
        t_ix = st:min(st + duration - 1, simulation_length_in_ms)
        dynomat[dyno, t_ix] = dynomat[dyno, t_ix] + 1
      }
    }
    # otherwise the request will be queued
    else {
      # 'intelligent' queueing will assign the request to the next dyno that comes available
      if(is.na(dyno) && router_mode == "intelligent") {
        routerrow = routermat[router,]
        dyno = sample(which(routerrow == min(routerrow)), 1)
        # again have to sample here instead of which.min()
      }
      
      if(router_mode == "naive" & unicorn) {
        dyno = as.numeric(sub_dynos[which.min(dyno_next_available[sub_dynos])])
      }
      
      queue_time = dyno_next_available[dyno] - st
      results[i, "time_in_queue"] = queue_time
      results[i, "end_time"] = st + queue_time + duration - 1
      dyno_next_available[dyno] = st + queue_time + duration
      
      if(router_mode == "intelligent") {
        routermat[router, dyno] = st + queue_time + duration
      }
      
      if(track_dyno_queues) {
        t_ix = st:min(st + queue_time + duration - 1, simulation_length_in_ms)
        dynomat[dyno, t_ix] = dynomat[dyno, t_ix] + 1
      }
    }
  }
  
  return(results)
}

frac_queued = function(result) {
  qtimes = result[, "time_in_queue"]
  
  data.frame(frac_queued = round(mean(qtimes > 0), 3),
             mean_queue_time_when_queued = round(mean(qtimes[qtimes > 0])),
             mean_queue_time_total_per_request = round(mean(qtimes)),
             median_queue_time_when_queued = round(as.numeric(median(qtimes[qtimes > 0]))),
             median_queue_time_total_per_request = round(as.numeric(median(qtimes))))
}

get_results = function(router_mode, dyno_count, choice_of_two = FALSE, unicorn_workers_per_dyno = 0, power_of_two = FALSE, parallel_router_count = 1) {
  tmp = frac_queued(run_simulation(router_mode = router_mode, dyno_count = dyno_count, choice_of_two = choice_of_two, power_of_two = power_of_two, unicorn_workers_per_dyno = unicorn_workers_per_dyno, reqs_per_minute = 9000, simulation_length_in_minutes = 5, parallel_router_count = parallel_router_count))
  
  opts = if(choice_of_two) {
    "choice of two"
  } else if(power_of_two) {
    "power_of_two"
  } else if(unicorn_workers_per_dyno > 0) {
    paste("unicorn", unicorn_workers_per_dyno, "workers per dyno", sep=" ")
  } else {
    ""
  }
  
  tmp$type = paste(router_mode, opts, dyno_count, sep=" ")
  tmp$router_type = router_mode
  tmp$dyno_count = dyno_count
  tmp$router_count = parallel_router_count
  return(tmp[, c(6:9, 1:5)])
}

results = rbind(get_results(router_mode = "intelligent", dyno_count = 15),
                get_results(router_mode = "intelligent", dyno_count = 20),
                get_results(router_mode = "intelligent", dyno_count = 25),
                get_results(router_mode = "naive", dyno_count = 25),
                get_results(router_mode = "naive", dyno_count = 25, choice_of_two = TRUE),
                get_results(router_mode = "naive", dyno_count = 25, power_of_two = TRUE),
                get_results(router_mode = "naive", dyno_count = 25, unicorn_workers_per_dyno = 2),
                get_results(router_mode = "naive", dyno_count = 50),
                get_results(router_mode = "naive", dyno_count = 50, choice_of_two = TRUE),
                get_results(router_mode = "naive", dyno_count = 50, power_of_two = TRUE),
                get_results(router_mode = "naive", dyno_count = 50, unicorn_workers_per_dyno = 2),
                get_results(router_mode = "naive", dyno_count = 100),
                get_results(router_mode = "naive", dyno_count = 100, choice_of_two = TRUE),
                get_results(router_mode = "naive", dyno_count = 100, power_of_two = TRUE),
                get_results(router_mode = "naive", dyno_count = 100, unicorn_workers_per_dyno = 2))

results


public class PercolationStats {
    private    int T;
    private    double[] threshold;
    private    double mu = 0;
    private    double dev = 0;


    public PercolationStats(int N, int T) { 
        // perform T independent computational experiments on an N-by-N grid
        if (N <= 0  || T <= 0) 
            throw new IllegalArgumentException("N or T are Illegal");
        this.threshold = new double[T];
        this.T = T;

        int[] samples = new int[N*N];
        for (int i = 0; i < N*N; i++) {
            samples[i] = i;
        }
        // Shuffling samples; 

        Percolation perco;
        for (int j = 0; j < T; j++) { 
            perco = new Percolation(N);
            StdRandom.shuffle(samples);
            int nosamples = 0;
            for (int k = 0; k < N*N; k++) { 
                int[] point = to2D(samples[k], N);
                int p = point[0];
                int q = point[1];
                perco.open(p, q);
                nosamples += 1;
                if (perco.percolates()) break;
            } 
            threshold[j] = nosamples/(double) (N*N);
            
        }
        StdOut.println("Percolation mean: " + mean());
        StdOut.println("Percolation stddev: " + stddev());
    }

    public double mean() {
        // sample mean of percolation threshold
        double mean = 0.0;
        for (int i = 0; i < T; i++) { 
            mean += this.threshold[i];
        }
        mu = mean/T;
        return mu;
    }

    public double stddev() {
        // sample standard deviation of percolation threshold
        double sttdev = 0.0;
        if (mu == 0 ) mu = mean();
        for (int i = 0; i < T; i++) { 
            sttdev += (threshold[i] - mu) * (threshold[i] - mu);
        }
        dev = Math.sqrt(sttdev/(double) (T-1));
        return dev;
    }

    public double confidenceLo() {
        // returns lower bound of the 95% confidence interval
        return (mean() - (stddev()*1.96)/Math.sqrt(T));
    }

    public double confidenceHi() {
        // returns upper bound of the 95% confidence interval
        return (mean() + (stddev()*1.96)/Math.sqrt(T));
    }

    /*
     *method to translate 1D models for 2D to shuffle randomly
     */
    private int[] to2D(int idx, int N) {
        int[] result = new int[2];
        result[0] = idx % N + 1;
        result[1] = idx/N + 1;
        return result;
    }

    public static void main(String[] args) {
        int n = Integer.parseInt(args[0]);
        int t = Integer.parseInt(args[1]);
        new PercolationStats(n, t);
    }

}

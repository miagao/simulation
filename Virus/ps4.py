# 6.00.2x Problem Set 4

import numpy
import random
import pylab
from ps3b import *

#
# PROBLEM 1
#        
def simulationDelayedTreatment(numTrials):
    """
    Runs simulations and make histograms for problem 1.

    Runs numTrials simulations to show the relationship between delayed
    treatment and patient outcome using a histogram.

    Histograms of final total virus populations are displayed for delays of 300,
    150, 75, 0 timesteps (followed by an additional 150 timesteps of
    simulation).

    numTrials: number of simulation runs to execute (an integer)
    """
    numViruses, maxPop, maxBirthProb, clearProb, resistances, mutProb = 100, 1000, 0.1, 0.05, {'guttagonol': False}, 0.005
    tests = [{'guttagonol': False}, {'guttagonol': True}]
    delay = 150
    
    print "Variando resistances"    
    for resistances in tests:
        final_pop = []        
        
        population = [0 for x in range(delay+150)]
        resistant = [0 for x in range(delay+150)]
        for trial in range(numTrials):
 

            viruses = []
            for i in range(numViruses):
                virus = ResistantVirus(maxBirthProb,clearProb,resistances, mutProb)
                viruses.append(virus)
                
            patient = TreatedPatient(viruses,maxPop)
            for j in range(delay):
                population[j] += patient.update()
                resistant[j] += patient.getResistPop(["guttagonol"])
            
            patient.addPrescription("guttagonol")
            for k in range(delay,delay + 150):
                if k == delay + 149:
                    aux = patient.update()
                    final_pop.append(aux)
                    population[k] += aux
                else:
                    population[k] += patient.update()
                resistant[k] += patient.getResistPop(["guttagonol"])
            
            
            
            
        for i in range(delay+150):
            population[i] = float(population[i])/numTrials
            resistant[i] = float(resistant[i])/numTrials

    
        

        pylab.plot(population, label = 'population')
        pylab.plot(resistant, label = 'resistant')
        pylab.title("SimpleVirus simulation")
        pylab.xlabel("Time Steps")
        pylab.ylabel("Average Virus Population")
        pylab.legend()
        pylab.show()
        pylab.hist(final_pop)
        pylab.show()





#==============================================================================
#simulationDelayedTreatment(100)
#==============================================================================
#
# PROBLEM 2
#
def simulationTwoDrugsDelayedTreatment(numTrials):
    """
    Runs simulations and make histograms for problem 2.

    Runs numTrials simulations to show the relationship between administration
    of multiple drugs and patient outcome.

    Histograms of final total virus populations are displayed for lag times of
    300, 150, 75, 0 timesteps between adding drugs (followed by an additional
    150 timesteps of simulation).

    numTrials: number of simulation runs to execute (an integer)
    """

    numViruses, maxPop, maxBirthProb, clearProb, resistances, mutProb = 100, 1000, 0.1, 0.05, {'guttagonol': False, 'grimpex': False}, 0.005
    
    delays = [300,150,75,0]
    for delay in delays:
        final_pop = []        
        
        population = [0 for x in range(delay+300)]
        resistant_guttagonol = [0 for x in range(delay+300)]
        resistant_grimpex = [0 for x in range(delay+300)]
        for trial in range(numTrials):
 

            viruses = []
            for i in range(numViruses):
                virus = ResistantVirus(maxBirthProb,clearProb,resistances, mutProb)
                viruses.append(virus)
                
            patient = TreatedPatient(viruses,maxPop)
            for j in range(150):
                population[j] += patient.update()
                resistant_guttagonol[j] += patient.getResistPop(["guttagonol"])
                resistant_grimpex[j] += patient.getResistPop(["grimpex"])
                
            patient.addPrescription("guttagonol")
            for k in range(150,150+delay):
                population[k] += patient.update()
                resistant_guttagonol[k] += patient.getResistPop(["guttagonol"])
                resistant_grimpex[k] += patient.getResistPop(["grimpex"])
                
            patient.addPrescription("grimpex")
            for k in range(150+delay,150+delay+150):
                if k == delay + 299:
                    aux = patient.update()
                    final_pop.append(aux)
                    population[k] += aux
                else:
                    population[k] += patient.update()
                resistant_guttagonol[k] += patient.getResistPop(["guttagonol"])
                resistant_grimpex[k] += patient.getResistPop(["grimpex"])
            
            
            
            
        for i in range(delay+300):
            population[i] = float(population[i])/numTrials
            resistant_guttagonol[i] = float(resistant_guttagonol[i])/numTrials
            resistant_grimpex[i] = float(resistant_grimpex[i])/numTrials

    
        

        pylab.plot(population, label = 'population')
        pylab.plot(resistant_guttagonol, label = 'resistant guttagonol')
        pylab.plot(resistant_grimpex, label = 'resistant grimpex')
        pylab.title("SimpleVirus simulation")
        pylab.xlabel("Time Steps")
        pylab.ylabel("Average Virus Population")
        pylab.legend()
        pylab.show()
        pylab.hist(final_pop)
        pylab.show()

#==============================================================================
simulationTwoDrugsDelayedTreatment(10)
#==============================================================================

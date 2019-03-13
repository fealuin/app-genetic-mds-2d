library('irace')
setwd('~/Documents/Projects/u/memoria/app-genetic-mds-2d/src/irace/tuning')
parameters=readParameters(file='parameters.txt')

scenario <- readScenario(filename = "scenario.txt",
                         scenario = defaultScenario())
irace(scenario = scenario,parameters = parameters)
testing.main(logFile =  'irace.log.Rdata')



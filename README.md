# Health Data Platform B.C. (HDPBC) Data Profiling

__* Code only, no outputs *__

This is example code for profiling the data holdings housed on the [Health Data Platform B.C. (HDPBC)](https://www2.gov.bc.ca/gov/content/health/conducting-health-research-evaluation/data-access-health-data-central/health-sector-partner-access-to-data/health-data-platform). If you are interested in working with HDPBC, you can learn about how to apply for access [here](https://www2.gov.bc.ca/gov/content/health/conducting-health-research-evaluation/data-access-health-data-central/health-sector-partner-access-to-data/health-data-platform/accessing-hdp).

In this example, most of the profiling is geared towards the [NACRS data holding](https://assets-hdp.healthbc.org/Dataset/details/d69f3377-6196-42e0-9f41-b303fd3ec4ae) except for code checking attributes with a hierarchical relationship, which works with the [HealthLink 811 data holding](https://assets-hdp.healthbc.org/Dataset/details/80844660-2c99-4c16-82d0-0c89e1333d19). 

This code incorporates a yaml configuration file, as well as several R files that contain customized functions and data rules. If a different user were to adopt this code, they would need to edit the yaml file to suit their needs (most importantly, changing the username to their own).

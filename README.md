system-stats
============

Collection of functions to collect system statistics (load/cpu %/memory %)

#### Example:

    1> system_stats_utils:top("28636").
    vsize: 4221.9M rss: 1227.3M cpu: 1329.5% mem: 1.91%
    vsize: 4222.2M rss: 1227.3M cpu: 1297.0% mem: 1.91%
    vsize: 4221.9M rss: 1226.8M cpu: 1300.2% mem: 1.91%
    vsize: 4240.4M rss: 1236.3M cpu: 1364.8% mem: 1.92%
    vsize: 4240.4M rss: 1236.6M cpu: 1450.5% mem: 1.92%
    vsize: 4240.4M rss: 1236.6M cpu: 1617.1% mem: 1.92%
    
#### Supported OS:

+ Linux (2.6.24+)

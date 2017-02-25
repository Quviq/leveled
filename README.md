# LevelEd - An Erlang Key-Value store

## Introduction

LevelEd is a work-in-progress prototype of a simple Key-Value store based on the concept of Log-Structured Merge Trees, with the following characteristics:

- Optimised for workloads with larger values (e.g. > 4KB).

- Explicitly supports HEAD requests in addition to GET requests. 
  - Splits the storage of value between key/metadata and body, 
  - allowing for HEAD requests which have lower overheads than GET requests, and
  - queries which traverse keys/metadatas to be supported with fewer side effects on the page cache.

- Support for tagging of object types and the implementation of alternative store behaviour based on type.
  - Potentially usable for objects with special retention or merge properties.

- Support for low-cost clones without locking to provide for scanning queries.
  - Low cost specifically where there is a need to scan across keys and metadata (not values).

- Written in Erlang as a message passing system between Actors.

The store has been developed with a focus on being a potential backend to a Riak KV database, rather than as a generic store.  

The primary aim of developing (yet another) Riak backend is to examine the potential to reduce the broader costs providing sustained throughput in Riak i.e. to provide equivalent throughput on cheaper hardware.  It is also anticipated in having a fully-featured pure Erlang backend may assist in evolving new features through the Riak ecosystem  which require end-to-end changes, rather than requiring context switching between C++ and Erlang based components.

The store is not expected to offer lower median latency than the Basho-enhanced leveldb, but it is likely in some cases to offer improvements in throughput, reduced tail latency and reduced volatility in performance.

## More Details

For more details on the store:

- An [introduction](docs/INTRO.md) to LevelEd covers some context to the factors motivating design trade-offs in the store.

- The [design overview](docs/DESIGN.md) explains the actor model used and the basic flow of requests through the store.

- [Future work](docs/FUTURE.md) covers new features being implemented at present, and improvements necessary to make the system production ready.

- There is also a ["Why"](docs/WHY.md) section looking at lower level design choices and the rationale that supports them.

## Is this interesting?

Making a positive contribution to this space is hard - given the superior brainpower and experience of those that have contributed to the KV store problem space in general, and the Riak backend space in particular.

The target at inception was to do something interesting, to re-think certain key assumptions and trade-offs, and prove through working software the potential for improvements to be realised.

[Initial volume tests](docs/VOLUME.md) indicate that it is at least interesting.  With improvements in throughput for multiple configurations, with this improvement becoming more marked as the test progresses (and the base data volume becomes more realistic).  

The delta in the table below  is the comparison in Riak performance between the identical test run with a Leveled backend in comparison to Leveldb.

Test Description                  | Hardware     | Duration |Avg TPS    | Delta (Overall)  | Delta (Last Hour)
:---------------------------------|:-------------|:--------:|----------:|-----------------:|-------------------:
8KB value, 60 workers, sync       | 5 x i2.2x    | 4 hr     |           |                  |
8KB value, 100 workers, no_sync   | 5 x i2.2x    | 6 hr     | 14,100.19 | <b>+ 16.15%</b>  | <b>+ 35.92%</b>
8KB value, 50 workers, no_sync    | 5 x d2.2x    | 6 hr     | 10,400.29 | <b>+  8.37%</b>  | <b>+ 23.51%</b> 

Tests generally show a 5:1 improvement in tail latency for LevelEd.

All tests have in common:

- Target Key volume - 200M with pareto distribution of load
- 5 GETs per 1 update 
- RAID 10 (software) drives
- allow_mult=false, lww=false
- modified riak optimised for leveled used in leveled tests


The throughput in leveled is generally CPU-bound, whereas in comparative tests for leveledb the throughput was disk bound.  This potentially makes capacity planning simpler, and opens up the possibility of scaling out to equivalent throughput at much lower cost (as CPU is relatively low cost when compared to disk space at high I/O) - [offering better alignment between resource constraints and the cost of resource](docs/INTRO.md).

More information can be found in the [volume testing section](docs/VOLUME.md).

As a general rule though, the most interesting thing is the potential to enable [new features](docs/FUTURE.md).  The tagging of different object types, with an ability to set different rules for both compaction and metadata creation by tag is considered to be a potential enabler for changes, as well as having a separate key store which can be scanned without breaking the page cache or working against mitigation for write amplification.

## Next Steps

Further volume test scenarios are the immediate priority, in particular volume test scenarios with:

- Alternative object sizes;

- Significant use of secondary indexes;

- Use of newly available [EC2 hardware](https://aws.amazon.com/about-aws/whats-new/2017/02/now-available-amazon-ec2-i3-instances-next-generation-storage-optimized-high-i-o-instances/) which potentially is a significant changes to assumptions about hardware efficiency and cost.

- Create riak_test tests for new Riak features enabled by LevelEd.

## Feedback

Please create an issue if you have any suggestions.  You can ping me @masleeds if you wish
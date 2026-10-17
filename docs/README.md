# Developer documentation

Start with the [repository guide](../README.md) for setup and the
[documentation site](../docs-site/README.md) for SDK, operator, watcher, and
on-chain guides. This directory holds the specifications, decisions, acceptance
criteria, and evidence that support those guides.

| Need                                                       | Maintained source                                            |
| ---------------------------------------------------------- | ------------------------------------------------------------ |
| Protocol design target                                     | [Technical specification](../technical-spec/README.md)       |
| Concrete transaction format and carriage                   | [Component specifications](spec/README.md)                   |
| Deployment tuple, compiled limits, and release obligations | [Consensus profile](consensus-profile-v1.md)                 |
| Public deployment acceptance                               | [Public-testnet readiness](public_testnet_readiness.md)      |
| Proof coverage and verification evidence                   | [Fault-proof guide](fault-proofs/README.md)                  |
| Protocol and SDK decisions                                 | [Protocol ADRs](midgard/decisions/)                          |
| Fault-proof decisions                                      | [Fault-proof ADRs](fault-proofs/decisions/README.md)         |
| Remaining canonical implementation and proof acceptance    | [Canonical execution specification](exec-plans/GOAL_SPEC.md) |
| Test-quality remediation                                   | [Task index](test-quality/README.md)                         |
| Reproducible performance checks                            | [Benchmark scenarios](benchmark-scenarios/)                  |
| Agent workflow and domain guidance                         | [Agent guide](agents/README.md)                              |

Read [the documentation policy](DOCUMENTATION_POLICY.md) before changing an
implementation claim or retiring a plan. Completed task diaries belong in Git
history; lasting decisions belong in ADRs, current procedures in guides, and
required verification inputs beside their tools. Every retained evidence artifact
names a current consumer or requirement; historical context alone belongs in Git.

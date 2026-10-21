# Validation-trace defense

The node defends the blocks it accepted against validation-trace dispute
cases. When a case opens against an unmerged block the node accepted (its own
blocks and every ancestor it built on), the node replays the disputed event
from its retained DA payload and its landed ledger. If the replay accepts the
event, the node defends the case and plays its side of the bisection game
until the case's duration ends. If the replay rejects the event, the block is
at fault: the node names that and does not defend.

The defender runs as the `validationTraceDefense` fiber in the node process.
It needs the L1 follower (see the [node README](../README.md)); without a
follower view it does nothing and names nothing. It needs no configuration of
its own. It signs with the operator key (`L1_OPERATOR_SEED_PHRASE`).

## Funding

A Defend locks, for each case:

- the thread floor: the fraud-proof output's lovelace plus a fee reserve for
  every charged move of the thread (82.4 ADA on the current profiles);
- up to 5 ADA left in the defense pocket, for the thread's moves to use as
  collateral when the pocket holds no plain-ADA output to lend;
- its own transaction fee.

The thread's lovelace comes back to the defense pocket, less the moves' fees,
when the thread expires after the case's maximum duration. A move whose fee
is over the per-move cap is paid wholly from the wallet and the pocket.

Keep the operator wallet at least **2 × (82.4 + 5) ≈ 175 ADA** above its
normal commit float, so two cases can be defended at the same time without
touching the ADA that commits need. An item the wallet and the pocket cannot
fund is named `validation-trace-defense-underfunded` with the shortfall, and
fails `/readyz` until it is funded.

A Defend also holds the defender's operator bond until the case is over
(`started_at + D_case + P`); recovering that bond waits until then.

## The defense pocket

The defense pocket is the Enterprise address of the operator's payment key:
the same key as the operator wallet (a Base address), without the stake
part. A thread's leftover and its expiry pay the pocket. The node tracks the
pocket as one of its own wallets, so its value stays in the node's view, and
defense moves spend from the wallet and the pocket together. The operator key
controls the pocket; move its ADA back to the wallet with that key when it is
not needed for a live case.

## Commits during a Defend

A due Defend pauses block commits once, for at most three minutes plus two
dispute validity ranges past when the Defend could first be sent, so the
Defend can take the L1 control plane. The commit fiber logs
`skipped_due_validation_trace_defense` while it waits. Commits never stop for
longer: after the bound they run whether or not the Defend has landed.

## Named states and `/readyz`

Each tick records the defender's named states. `/readyz` lists each as
`<state>:<case or thread unit>:<deadline|none>[:<shortfall lovelace>]`, under
its reasons when the state fails readiness and under its details otherwise.

| State                                            | Meaning                                                                                                     | `/readyz`                                                |
| ------------------------------------------------ | ----------------------------------------------------------------------------------------------------------- | -------------------------------------------------------- |
| `validation-trace-defense-awaiting`              | Our thread waits for the challenger's move.                                                                 | detail                                                   |
| `validation-trace-defense-one-step`              | Our thread reached the one-step resolution.                                                                 | detail                                                   |
| `validation-trace-defense-won-pending-expiry`    | The challenger ran out of clock; our thread waits for its expiry.                                           | detail                                                   |
| `validation-trace-defense-thread-lost`           | Our thread ran out of clock or lost its resolution. No operator step changes it.                            | detail                                                   |
| `validation-trace-defense-operator-fault`        | The replay rejects the event the block accepted: the block is at fault, and the node does not defend it.    | detail                                                   |
| `validation-trace-defense-history-pruned`        | The case was already defended and its history is past the follower's retention, so the node cannot join it. | detail                                                   |
| `validation-trace-defense-funding-pending`       | Another live defense transaction of ours holds what funds this move, until it lands or dies.                | detail                                                   |
| `validation-trace-defense-material-unavailable`  | The node cannot build its defense yet (for example, a retained payload is missing); it retries every tick.  | fails within a quarter of the side clock of its deadline |
| `validation-trace-defense-trace-disagrees`       | Our replay does not reproduce an accepted run. Investigate the node's replay.                               | fails within a quarter of the side clock of its deadline |
| `validation-trace-defense-references-unresolved` | A published reference script the move needs cannot be read.                                                 | fails within a quarter of the side clock of its deadline |
| `validation-trace-defense-underfunded`           | The wallet and the pocket lack the shortfall for the move. Fund the operator wallet.                        | fails                                                    |
| `validation-trace-defense-not-active`            | A Defend is due but the operator has no active node. Activate the operator.                                 | fails                                                    |
| `validation-trace-defense-late`                  | A Defend is due within a quarter of the side clock and has not landed.                                      | fails                                                    |

A fiber whose ticks keep failing for more than ten dispute validity ranges
adds `validation-trace-defense-stalled:<consecutive failures>` to the reasons.
A failing tick never stops the node: it is recorded and retried at the next
tick.

## Upgrading

The release that adds the defender also adds the dispute's holders to the
L1 follower's tracked set. At the first start after the upgrade the follower
resets its store and replays from the origin; the node is unready with
`tracked_set_changed` and performs no duties until the replay reaches the tip.
It needs no operator step, and a restart resumes the replay. Schedule the
upgrade for a quiet period, with no forced transaction near its inclusion
deadline (see the [node README](../README.md)).

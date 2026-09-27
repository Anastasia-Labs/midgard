# Does cardano-node re-inject rolled-back txs into its mempool?

**Verdict: DOES NOT RE-INJECT.**

The research used shallow clones read on 2026-09-26. No repository was modified.

| Repo | HEAD SHA | Commit date |
|---|---|---|
| IntersectMBO/ouroboros-consensus | `7d630e8e54e7df7f694185f231be4b5c83b84388` | 2026-09-22 |
| IntersectMBO/ouroboros-network | `a3d8017e798b225055aaf9118ad062fe58bc650f` | 2026-09-15 |
| IntersectMBO/cardano-node | `19d9e5d6aa55d467705d8a277f0c84a4b2032823` | 2026-09-24 |
| cardano-foundation/cardano-wallet | `92b57304dfb556d80edf30d75e3f19218e66e891` | 2026-09-16 |

URL pattern: `https://github.com/<repo>/blob/<sha>/<path>#L<n>`.

## Q1: Does anything walk abandoned blocks' txs back into the mempool? No.

1. **Mempool sync trigger.** See `ouroboros-consensus/src/ouroboros-consensus/Ouroboros/Consensus/Mempool/Init.hs` L49–83. <!-- doc-links:external -->
   - `openMempool` forks `forkSyncStateOnTipPointChange`.
   - That is a `forkLinkedWatcher` on the ledger tip `Point`.
   - Its only action is `implSyncWithLedger (const ()) menv` (L77).
   - The watcher gets only the new tip point. It does not get the rolled-back blocks or their bodies.
2. **What sync revalidates.** See `.../Mempool/Update.hs` `implSyncWithLedger` (L526 onward).
   - The seed revalidation is `revalidateTxsFor frk ... (TxSeq.toList (isTxs is0))` (L579–588).
   - Its input is exactly the txs **already in the mempool** (`isTxs` of the snapshot `is0`).
   - The follow-up delta loop only reapplies txs added to the mempool since then, by `TicketNo` (`deltaAfter`).
   - No block contents are read anywhere in this path.
3. **Who can call `addTx` / `addTxs` in production.** A repo-wide grep, excluding test and bench, finds only:
   - `ouroboros-consensus/.../MiniProtocol/LocalTxSubmission/Server.hs` L33: `addLocalTxs`, for local clients.
   - `ouroboros-consensus-diffusion/.../Ouroboros/Consensus/NodeKernel.hs` L608–625: `getMempoolWriter` → `mempoolAddTxs = addTxs mempool txs`. This is the inbound side of NTN TxSubmission.
   - `ouroboros-consensus-cardano/src/unstable-cardano-tools/Cardano/Tools/DBAnalyser/Analysis.hs` L959: `addTxs mempool $ extractTxs blk'`. This is an offline db-analyser benchmark, not the node. <!-- doc-links:external -->
4. **`extractTxs` is tooling-only by contract.** See `.../Ledger/SupportsMempool.hs` L304–310: "Collect all transactions from a block. This is used for tooling only. We don't require it as part of RunNode."
   - In cardano-node, its only use is a tracer: `cardano-node/src/Cardano/Node/Tracing/Tracers/NodeToNode.hs` L61, which logs txIds. <!-- doc-links:external -->
5. **ChainDB is not coupled to the mempool.** `.../Storage/ChainDB/Impl/ChainSel.hs` contains 0 occurrences of "mempool", and the same holds for all of `ChainDB/Impl/*.hs`.
   - The only coupling is indirect: ChainDB changes the ledger tip, and the mempool watcher above polls that tip.

## Q2: What happens to txs already in the mempool on a switch? They are revalidated and invalid ones are dropped.

- `Mempool/API.hs` L118–131 (doc on `addTx`) says txs "valid in an older ledger state but are invalid in the current ledger state, could exist within the mempool until they are revalidated and dropped from the mempool via … the background thread that watches the ledger for changes". <!-- doc-links:external -->
- `Update.hs` L665–673 revalidates, traces `TraceMempoolRemoveTxs` for the removed txs, and commits the surviving candidate.
- Consensus docs, `docs/website/contents/explanations/data_flow.md` L114–116: "When the ledger state changes (e.g., after adopting a new chain), the mempool revalidates all buffered transactions." <!-- doc-links:external -->
- Why an included tx leaves the mempool: once a block containing tx T is adopted, revalidating T against the new ledger fails because its inputs are already spent, so T is dropped. There is no separate "remove included txs" step.
  - This is my inference from the revalidate-and-drop design.
  - The tech report `docs/tech-reports/report/chapters/future/misc.tex` L19 supports it: txs "will either be included in the blockchain or else will be chucked out because some of their inputs will have been used". <!-- doc-links:external -->

## Q3: Can a tx from an abandoned block propagate again or land again without a resubmission?

**Nothing pushes it back.** `ouroboros-network/lib/Ouroboros/Network/TxSubmission/Outbound.hs` shows that outbound tx-submission serves only what is in the local mempool: <!-- doc-links:external -->
- L139–163 announce ids with `mempoolTxIdsAfter lastIdx` on the current snapshot.
- L175–187 look txs up with `mempoolLookupTx`, and the comment says: "will return nothing if the transaction is no longer in the mempool. This is good. Neither the sending nor receiving side wants to forward txs that are no longer of interest."
- `TxSubmission/Mempool/Reader.hs` has the same design: a snapshot of current mempool contents only. <!-- doc-links:external -->

So a node that adopted the losing block and dropped T will never re-announce T.

The inbound V2 dedupe cache `bufferedTxsMinLifetime = 2` s (`Inbound/V2/Policy.hs` L98) is irrelevant on a multi-block rollback timescale. <!-- doc-links:external -->

**Caveat: T can still land again without the submitter doing anything.** This is my inference from the same code, not something the sources state.
- Mempools are per node.
- A node that **never adopted** the abandoned block may still hold T in its mempool. Examples: the producer of the winning fork, or any peer that stayed on the winning chain.
- On the winning chain, T is still valid if its inputs are unspent and its TTL has not passed. Such a node keeps T, may forge it into a block, and re-announces it via tx-submission.
- Nodes that dropped T will accept it again, because `mempoolHasTx` is false after the drop.
- So T often lands again after short forks, but this is opportunistic. It depends entirely on some mempool that never saw the losing block, and nothing guarantees it.

## Q4: Documentation that the client must resubmit

cardano-wallet, `specifications/design-notes/tx-resubmission.md` L11–28 (https://github.com/cardano-foundation/cardano-wallet/blob/92b57304dfb556d80edf30d75e3f19218e66e891/specifications/design-notes/tx-resubmission.md): <!-- doc-links:external -->

> "If there is a chain switch, `cardano-wallet` will roll back its transaction history database, and the transaction will revert back to _Pending_ state … it's not guaranteed that the transaction will ever appear again … The reason for this is that `cardano-node` doesn't track transactions which left its mempool due to TxSubmission, but were never adopted. … The same behaviour even applies to transactions which entered the node's mempool via LocalTxSubmission … So `cardano-wallet` itself must retry submission of _Pending_ transactions, for as long as their slot validity period is current."

- The wallet resubmits every ~10 blocks' worth of slots (L36–41).
- The wallet state diagram has `in_ledger --> pending: rollback`: `docs/site/src/design/concepts/transaction-lifecycle.md` L13. <!-- doc-links:external -->

## Not verified

- **No CIP found** that states the resubmission obligation. I did not search the CIPs repo.
- **No consensus issue or PR found** that explicitly discusses and rejects re-adding rolled-back txs. `gh search` on ouroboros-consensus, ouroboros-network and cardano-node returned nothing relevant. The conclusion rests on the absence of any code path, not on a stated design decision.
- **Only current HEADs were read.** The sync logic shown includes a recent off-lock refactor. Older released node versions were not checked, although they use the same "revalidate `isTxs` only" design by the same API contract.
- **Inbound V1 was not read line by line.**
- **Nothing was tested live.** The "can still land via a node that never saw the losing block" point is reasoned from code, not observed.

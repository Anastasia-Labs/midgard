# L1 fact store schema draft (node-l1-rollback-redesign.md sections 3 / 4.A)

Branch colll78/canonical-v1-watcher-l1-source-checkpoint, read-only. All paths
are relative to `demo/midgard-node/src/` unless prefixed. "SDK" = `demo/midgard-sdk/src/`.

Legend for "served by":
- `O.col` = l1_outputs column, `T.col` = l1_txs column, `B.col` = l1_blocks column
- `C` = kind-C content fetch (by-hash, verified), `X` = not servable from a tracked-address store
- `LSQ` = keep an Ogmios local-state-query / submit call (not a fact-store concern)

---------------------------------------------------------------------------

## A. Consumers -> fields read -> served by

### A.1 Event-history chain follower (writes facts today, becomes the follower)

| file:line | fields read | served by |
|---|---|---|
| l1-event-history-chain.ts:225-248 | block `type=="praos"`, `id`, `slot`, `height`, `ancestor` (rollback path) | B.hash, B.slot, B.height, B.parent_hash |
| l1-event-history-chain.ts:258-266 | continuity: parent == prev id, slot > prev, height == prev+1 | B.parent_hash, B.slot, B.height |
| l1-event-history-chain.ts:50-77 | tip height (queryNetwork/tip bracketing blockHeight; v6 tip has no height) | LSQ (follower-internal) |
| l1-event-history-source.ts:413-448 | every tx in block, duplicate tx id rejection (430-435) | T.tx_hash PK |
| l1-event-history-list-replay.ts:203-211 | same continuity (parent, slot, height+1) | B.* |

### A.2 Transaction decoder (the projection the event-history family consumes)

`HistoryChainTransaction` (l1-event-history-transaction.ts:31-45):

| field | decoded at | served by |
|---|---|---|
| txHash | :31-45 | T.tx_hash |
| spends ("inputs"/"collaterals") | :31-45 | T.is_valid |
| inputs, compareOutRefs-sorted | :31-45 | T.inputs (sorted outref array) |
| references, sorted | :31-45 | T.reference_inputs |
| collaterals, sorted | :31-45 | T.collaterals |
| outputs (LedgerSnapshotOutput) | :31-45, empty-omitted in v7 :172-174 | O.* rows with created_tx = tx; T.output_count |
| collateralReturn, index = rawOutputs.length | :248 | O row with output_index = T.output_count; T.has_collateral_return |
| mint policy+name -> bigint | :31-45 | T.mint (jsonb) + l1_tx_mint_policies |
| withdrawals {account, networkId, credential{kind,hash}, amount}, ledger-ordered | :97-149 | T.withdrawals (jsonb, ledger order) |
| redeemers {purpose, index, cbor}, sorted, bounds-checked | :198-235 | T.redeemers (jsonb) |
| invalidBefore / invalidAfter (slots) | :236-258 | T.invalid_before, T.invalid_after |
| historyZeroWithdrawal (withdrawals + redeemers) | :263-285 | T.withdrawals, T.redeemers |

NOT read anywhere in this decoder: fee, required signers, certificates, metadata/auxiliary data, witness datums, witness scripts, ExUnits (beyond redeemer cbor), vkey witnesses, raw tx cbor.

### A.3 Ledger snapshot (bootstrap capture)

| file:line | fields read | served by |
|---|---|---|
| l1-ledger-snapshot.ts:15-23 | `LedgerSnapshotOutput` {txHash, outputIndex, address, assets, datum?, datumHash?, hasReferenceScript} | O.tx_hash, O.output_index, O.address, O.assets, O.datum, O.datum_hash, O.script_ref IS NOT NULL |
| l1-ledger-snapshot.ts:114 | `hasReferenceScript: parsed.script !== undefined` (drops bytes) | must keep bytes: O.script_ref / O.script_ref_type (needed by A.8) |
| l1-ledger-snapshot.ts:28-32, 129-224 | point {slot,id}, addresses, outputs; no creation slot, no bodies, no spent history | bootstrap rows with created_slot NULL (see B, C) |

### A.4 Event-history block stage / transitions / replay

| file:line | fields read | served by |
|---|---|---|
| l1-event-history-block-stage.ts:57-82 | reference resolution: not spent earlier in block, not created at/after this tx index; current tracked outputs, then same-block created, then resolveReference; tracked-historical contradiction throws (:77-80) | O live-at-slot query + O.created_tx_index; untracked historical -> C |
| l1-event-history-block-stage.ts:98-109 | inputs vs collaterals by `spends` | T.is_valid, T.inputs, T.collaterals |
| l1-event-history-block-stage.ts:110-122 | outputs vs collateralReturn | O rows + T.has_collateral_return |
| l1-event-history-transition.ts:77-92 | list node authentication by policy prefix, reject reference scripts | O.assets, O.script_ref |
| l1-event-history-transition.ts:126 | null if failed | T.is_valid |
| l1-event-history-transition.ts:160-172 | inputs[i] + spend redeemer whose datum = own index | T.inputs, T.redeemers |
| l1-event-history-transition.ts:173-181 | outputs[i] | O by (tx, index) |
| l1-event-history-transition.ts:182-188 | references[i] via resolveReference | T.reference_inputs -> O (tracked) or C |
| l1-event-history-transition.ts:189-196 | invalidAfter -> slotToUnixTime | T.invalid_after |
| l1-event-history-transition.ts:197-221 | mint filtered to list policy, mint redeemer at sorted-policy index | T.mint, T.redeemers |
| l1-event-history-transition.ts:222-247 | continueNode: key, address, assets, datum fields 0 and 3, protected_until | O.address, O.assets, O.datum |
| l1-event-history-transition.ts:255-279 | openOrder retention output: address, inline datum, no datumHash, no ref script, datumToHash == storage_datum_hash | O.address, O.datum, O.datum_hash, O.script_ref |
| l1-event-history-transition.ts:343-357 | hub reference: address, assets, inline datum == hubDatumCbor | O.* |
| l1-event-history-transition.ts:440-441 | RetireOrder confirmed/settlement reference indexes | T.reference_inputs -> O or C |
| l1-event-history-transition.ts:486 | `transaction.redeemers.indexOf(observer.redeemer)` | T.redeemers order |
| l1-event-history-transition.ts:500-533 | nonce input outref, inclusion time from invalidAfter | T.inputs, T.invalid_after |
| l1-event-history-provenance.ts:11-17, 245-251 | Placement {blockHash, slot, height, transactionHash, transactionIndex} | B.hash, B.slot, B.height, T.tx_hash, T.block_tx_index |
| l1-event-history-provenance.ts:266-268 | never-reuse of event key: canonical.has(label) | projection (kind D) over retained facts; requires retention (see G.3) |
| l1-event-history-list-replay.ts:27, 127-139, 141-148 | getCreatingBody(txHash) raw body CBOR for historical refs; receipt carries creatingBodies bytes | C (by-hash body, verified per l1-event-history-reference.ts:52-87) |
| l1-event-history-list-replay.ts:63-75 | isTrackedOutput: hub or list-policy asset | O.assets / l1_output_assets |
| l1-event-history-reference.ts:52-87 | raw BODY cbor: hash == ref.txHash, size <= maximumBodyBytes, CML round-trip; index < len -> outputs, index == len -> collateral_return | C only (raw body bytes) |
| services/event-history-owner.ts:451-489, 692, 754-771 | withBodies: lazy readEventHistoryCreatingBody on MissingBody | C |
| l1-event-history-activation.ts:60-82 | activation tx: spends, nonce input, mint (hub unit = 1, correction-lock unit = 1), hub output | T.is_valid, T.inputs, T.mint, O.* (if activation >= fact-store origin) else C |
| l1-event-history-transport.ts:261-290 (called services/event-history-owner.ts:880) | locate activation tx from hub outref's creating tx | O.created_tx / B (if in store) else C |
| l1-event-history-source.ts:216-247 | verify hub outputs: address, assets, datum, datumHash, hasReferenceScript | O.* |
| l1-event-history-ledger-projection.ts:61-70 | tracked scope = ledger.addresses (5 addresses) | O.address IN (...) live at point |

### A.5 State-queue correction observer (services/state-queue-correction-observer.ts)

| file:line | fields read | served by |
|---|---|---|
| :775-831 decodeKupoCorrectionLockMatch | address, assets (exactly one unit = lock, qty 1), datum_type inline, datum -> CorrectionLockDatum | O.address, O.assets, O.datum |
| :833-874 fetchKupoResolvedOutput (called :1000-1020 for every spent and reference input) | `matches/idx@tx?resolve_hashes`, exactly one match else throw | O by out_ref, spent or unspent (needs retained spent rows). Untracked inputs: miss; see F.6 |
| :876-931 fetchKupoTransactionCorrectionLockOutputs | `*@txHash` incl. spent: output_index, address, assets, inline datum | O WHERE tx_hash = ? (spent rows retained) |
| :933-977 fraudProofAssetNameFromResolvedMatch | address, inline datum, assets under fraud-proof policy, name.slice(8) = target header hash | O.address, O.datum, l1_output_assets |
| :1041-1071 | mintPolicyIds.indexOf(stateQueuePolicyId), mint redeemer at that index -> StateQueueRedeemer | T.mint (sorted policy ids), T.redeemers |
| :1192-1322 fetchKupoTransactionQueueOutputs | `*@txHash`: output_index, address, assets, inline datum (LinkedListDatum key, next) | O WHERE tx_hash = ? |
| :1435, 1465, 1486 canonicalDepth / fetchKupoSpend | spent tx id + spending block hash; tip blockNo | O.spent_tx, O.spent_slot -> B.hash, B.height; follower tip |
| :1519-1527 fetchKupoAncestorPoint + readOgmiosBlockTransaction | locate spending tx body in its block | T by hash (no chainsync rescan needed) |
| :1545-1582 transitionInput | transactionHash, blockHash, slot, blockNo, transactionIndex, mintPolicyIds, redeemers {purpose,index,cborHex}, spentInputOutRefs, referenceInputOutRefs | T.* + B.* |

### A.6 Tx-order carriage (l1-tx-order-carriage.ts)

| file:line | fields read | served by |
|---|---|---|
| :108-124 ObservedL1Transaction(+AtPoint) | txHash, spentInputs, referenceInputs, mintPolicyIds (sorted), redeemers {purpose,index,redeemer}; blockPoint {slot, headerHash, blockNo}, transactionIndex | T.* + B.* |
| :711-803 parseObservedTransaction | Ogmios JSON only; 700-710: include-transaction-cbor cannot be required of an operator endpoint | T decoded columns (no raw cbor needed) |
| :337-340 KupoSpend | point + transactionId only (Kupo input_index/redeemer buggy, 309-336) | O.spent_tx, O.spent_slot |
| :466-493 fetchKupoAncestorPoint (`/checkpoints/{slot-1}`) | predecessor point for chainsync | B.parent_hash (not needed once T is stored) |
| :935-962 txOrderMintRedeemer | mint redeemer at tx-order policy's sorted index | T.mint, T.redeemers |
| :968-997 txOrderMintCarriageVector | decode redeemer | T.redeemers |
| :1063-1110 resolveCarriageReferenceInputs | sorted ref inputs; datum_type, datum_hash, inline datum bytes of each ref; refs sit at the creator wallet (untracked, may be spent) | X from tracked store -> C / LSQ (F.1) |
| :1132-1152 | Kupo creation point of the order outref, ancestor, chainsync to the creating tx, then mint redeemer | T by O.created_tx if created after store origin; else C (F.2) |
| :1113-1174 | skip ref resolution when every entry is Inline | n/a |

### A.7 Tx-order ingestion (fibers/fetch-and-insert-tx-order-utxos.ts)

| file:line | fields read | served by |
|---|---|---|
| :85-106 -> SDK tx-order.ts:277 | utxosAt(txOrder address), policy filter, datum (inclusion_time from datum) | O.address, O.assets, O.datum |
| :344-350 | observeTxOrderMaterialCarriageProgram over the order outref | A.6 |
| :495-530, 558-570 | `utxosAt({type:"Script", hash: cekProgramMaterial.spendingScriptHash})` by PAYMENT CREDENTIAL; inline datum only | O.payment_cred index; bootstrap is X for LSQ (F.5) |
| database/forcedTransactions.ts | TX_ORDER_L1_TX_HASH, TX_ORDER_L1_OUTPUT_INDEX, RAW_DATUM, INCLUSION_TIME (own DB, no L1 read) | kind D |

### A.8 Lucid-provider consumers (through services/lucid.ts:110 provider)

Lucid UTxO = {txHash, outputIndex, address, assets, datumHash (hash-type only), datum (inline only), scriptRef {type, script}} (kupmiosUtxosToUtxos).
Every provider read below is served by O.* live-at-tip; `scriptRef` requires O.script_ref + O.script_ref_type.

| file:line | read | key |
|---|---|---|
| workers/commit-block-header/state-queue.ts:116-150, 159-257 -> SDK state-queue.ts:2009-2034 | utxosAt(stateQueue); datum da_attestation, header.endTime, key, next | address |
| workers/commit-block-header/state-queue.ts:275 | utxosAtWithUnit(stateQueue, unit) | address + unit |
| services/state-queue-topology.ts:123-138 | utxosAtWithPolicy (SDK.utxosAtByNFTPolicyId) | policy |
| services/state-queue-topology.ts:149-223 (:174) | utxosAtWithUnit walk from root, cap 10_000 (:140); SDK state-queue.ts:566-585 inline datum + asset name | address + unit |
| workers/utils/scheduler-refresh.ts:1370, 1567 | scheduler witness unit (SchedulerDatum), hub oracle unit | unit |
| transactions/da-attestation.ts:120, 208 | DA params unit (DaParamsDatum), DA attestation unit by header hash | unit |
| workers/commit-block-header.ts:1353 | state-queue node unit -> outref | unit |
| merge-to-confirmed-state.ts:590, 1069 | utxosAt(stateQueue) count; hub | address / unit |
| register-active-operator.ts:207, 219 | operator lists | address / unit |
| initialization.ts:532, 558, 619, 641, 671, 814 | singletons, lists | address / unit |
| SDK operator-lifecycle/directory.ts:187 | registered/active/retired lists, scheduler, hub, state-queue tail | address / unit |
| SDK correction-lock.ts:101, hub-oracle.ts:346 | single authentic UTxO by address + policy | address + policy |
| reference-scripts.ts:151, 434, 471, 503, 516, 582 | reference-script UTxOs (scriptRef bytes) | address, out_ref |
| reference-publication.ts:204-211 | reference address; scriptRef type + script bytes -> script hash | address; needs O.script_ref |
| reference-publication.ts:274, 321, 428, 529, 557 | utxosByOutRef of own wallet outputs | out_ref |
| event-history-submission.ts:282, 395; operator-wallet-view.ts:55; funding-preflight.ts:99 | wallet getUtxos | address (own wallets) |
| transactions/utils.ts:473 | required-output visibility: address, assets, datum/datumHash, scriptRef type+script | out_ref; used by reserve-payout.ts, reference-publication.ts, register-active-operator.ts:921, 1225 |
| transactions/utils.ts:384 + awaitTxConfirmation -> Kupmios getTransactionStatus (`/matches/*@txHash`) | tx confirmed iff it has an indexed output | T.tx_hash JOIN B (strictly better: works for txs with no tracked output) |
| attestation-timeout-correction.ts:284 | fraudProof address | address |
| commit-submission.ts:576, merge.ts:333, confirm-block-commitments.ts:133, 145 | stateQueueAuthValidator address | address |
| transactions/reference-publication-provider.ts:26-91 (:62) | Kupo `/checkpoints` caught up to Ogmios tip | follower cursor / B contains (tip.slot, tip.id) |
| services/native-ledger.ts:66-85 | getRewardAccount via local-node ledger (Ogmios omits undelegated registered script accounts) | LSQ, keep |
| provider getProtocolParameters (Lucid init), submitTx | | LSQ / submit, keep |
| evaluateTx | unused (localUPLCEval true at all 28 build sites) | n/a |
| getDelegation, getDatum | no use found in node or SDK src | n/a (getDatum would be X, datum-hash preimages are not facts) |

---------------------------------------------------------------------------

## B. Postgres DDL

```sql
-- Chain spine. One row per block the follower accepted on the current fork.
CREATE TABLE l1_blocks (
  slot         bigint  NOT NULL,              -- rollback point ordering; slotToUnixTime; Placement.slot
  hash         bytea   NOT NULL,              -- block id; rollback target identity; Placement.blockHash
  height       bigint  NOT NULL,              -- Placement.height, observer canonicalDepth, height+1 continuity
  parent_hash  bytea,                         -- continuity check (chain.ts:258-266); NULL only for the origin row
  PRIMARY KEY (slot),                         -- at most one block per slot on one fork
  UNIQUE (hash),
  UNIQUE (height)
);

-- One row per qualifying transaction (rule in E). Valid and phase-2-failed both.
CREATE TABLE l1_txs (
  tx_hash            bytea    PRIMARY KEY,   -- T.txHash; duplicate rejection (source.ts:430-435)
  block_slot         bigint   NOT NULL REFERENCES l1_blocks(slot) ON DELETE CASCADE, -- placement + rollback
  block_tx_index     integer  NOT NULL,      -- Placement.transactionIndex; same-block ordering (block-stage.ts:57-82)
  is_valid           boolean  NOT NULL,      -- spends inputs vs collaterals (transition.ts:126, block-stage.ts:98-122)
  inputs             bytea[]  NOT NULL,      -- 34-byte outrefs (32 hash || u16 index), ledger-sorted; redeemer spend pointers
  reference_inputs   bytea[]  NOT NULL,      -- sorted; transition.ts:182-188, observer :1016, carriage :1063
  collaterals        bytea[]  NOT NULL,      -- sorted; spent iff NOT is_valid
  output_count       integer  NOT NULL,      -- collateral-return index = output_count (transaction.ts:248, reference.ts:52-87)
  has_collateral_return boolean NOT NULL,    -- whether index output_count exists
  mint               jsonb    NOT NULL,      -- {policyHex: {nameHex: "qty"}}; policies in sorted order (mint pointer)
  withdrawals        jsonb    NOT NULL,      -- [{account, networkId, credential:{kind,hash}, amount}] ledger order (transaction.ts:97-149)
  redeemers          jsonb    NOT NULL,      -- [{purpose, index, cbor}] sorted (transaction.ts:198-235); indexOf at transition.ts:486
  invalid_before     bigint,                 -- slot, transaction.ts:236-258
  invalid_after      bigint,                 -- slot; inclusion time (transition.ts:189-196, 500-533)
  UNIQUE (block_slot, block_tx_index)
);

-- Policy side table so "which txs minted/burned policy P" is indexable.
CREATE TABLE l1_tx_mint_policies (
  tx_hash    bytea NOT NULL REFERENCES l1_txs(tx_hash) ON DELETE CASCADE,
  policy_id  bytea NOT NULL,                 -- mint-pointer lookup and policy scans
  PRIMARY KEY (tx_hash, policy_id)
);

-- One row per output at a tracked address (incl. collateral return), live or spent.
CREATE TABLE l1_outputs (
  tx_hash          bytea    NOT NULL,        -- out_ref part 1 (Lucid txHash)
  output_index     integer  NOT NULL,        -- out_ref part 2; == creating tx output_count for collateral return
  address          bytea    NOT NULL,        -- raw address bytes; every utxosAt / address equality check
  address_bech32   text     NOT NULL,        -- what Lucid and Kupmios consumers compare; avoids re-encoding in hot paths
  payment_cred     bytea,                    -- 28-byte hash; utxosAt({type:"Script",hash}) (fetch-and-insert-tx-order-utxos.ts:561); NULL for Byron
  payment_cred_is_script boolean,            -- Lucid credential type; NULL for Byron
  stake_cred       bytea,                    -- optional; nothing reads it today, cheap to keep for wallet views
  lovelace         numeric(20,0) NOT NULL,   -- assets.lovelace
  assets           jsonb    NOT NULL,        -- full multiasset {unit: "qty"}; exact assets equality (transition.ts:222-247, utils.ts:473)
  datum_hash       bytea,                    -- set iff hash-type datum (Lucid datumHash)
  datum            bytea,                    -- inline datum CBOR (Lucid datum); list/lock/queue/order decodes
  script_ref_type  text,                     -- 'PlutusV1'|'PlutusV2'|'PlutusV3'|'Native'; reference-publication.ts:204-209
  script_ref       bytea,                    -- script bytes; Lucid scriptRef, script-hash match, ref-script rejection
  created_slot     bigint REFERENCES l1_blocks(slot) ON DELETE CASCADE, -- rollback delete key; NULL = bootstrap row
  created_tx_index integer,                  -- same-block "created before tx i" rule (block-stage.ts:57-82)
  spent_slot       bigint REFERENCES l1_blocks(slot) ON DELETE SET NULL, -- rollback null-out key; live-at-slot
  spent_tx         bytea REFERENCES l1_txs(tx_hash) ON DELETE SET NULL,  -- observer fetchKupoSpend, carriage KupoSpend
  bootstrap_slot   bigint,                   -- snapshot point slot for rows with created_slot NULL
  PRIMARY KEY (tx_hash, output_index),
  CHECK ((created_slot IS NULL) = (created_tx_index IS NULL)),
  CHECK ((created_slot IS NULL) = (bootstrap_slot IS NOT NULL)),
  CHECK ((spent_slot IS NULL) = (spent_tx IS NULL)),
  CHECK (datum_hash IS NULL OR datum IS NULL),
  CHECK ((script_ref IS NULL) = (script_ref_type IS NULL))
);

-- Multiasset index table (jsonb stays the equality source of truth).
CREATE TABLE l1_output_assets (
  tx_hash       bytea   NOT NULL,
  output_index  integer NOT NULL,
  policy_id     bytea   NOT NULL,            -- utxosAtWithPolicy (state-queue-topology.ts:123-138)
  asset_name    bytea   NOT NULL,            -- utxosAtWithUnit, getUtxoByUnit (13 node call sites)
  quantity      numeric(40,0) NOT NULL,
  PRIMARY KEY (tx_hash, output_index, policy_id, asset_name),
  FOREIGN KEY (tx_hash, output_index) REFERENCES l1_outputs ON DELETE CASCADE
);

-- Follower cursor (single row); replaces Kupo /checkpoints sync (reference-publication-provider.ts:62).
CREATE TABLE l1_follower_cursor (
  id        boolean PRIMARY KEY DEFAULT true CHECK (id),
  slot      bigint NOT NULL,
  hash      bytea  NOT NULL,
  origin_slot bigint NOT NULL                -- first block the store has full tx facts for (bootstrap point P)
);

-- Indexes
CREATE INDEX l1_outputs_address_live   ON l1_outputs (address) WHERE spent_slot IS NULL;            -- utxosAt
CREATE INDEX l1_outputs_address_all    ON l1_outputs (address, created_slot);                        -- history scans
CREATE INDEX l1_outputs_payment_live   ON l1_outputs (payment_cred) WHERE spent_slot IS NULL;       -- utxosAt(credential)
CREATE INDEX l1_outputs_spent_tx       ON l1_outputs (spent_tx) WHERE spent_tx IS NOT NULL;         -- observer transitions
CREATE INDEX l1_outputs_created_slot   ON l1_outputs (created_slot);                                 -- rollback delete
CREATE INDEX l1_outputs_spent_slot     ON l1_outputs (spent_slot) WHERE spent_slot IS NOT NULL;     -- rollback null-out, GC
CREATE INDEX l1_output_assets_unit     ON l1_output_assets (policy_id, asset_name);                  -- unit lookup
CREATE INDEX l1_tx_mint_policy         ON l1_tx_mint_policies (policy_id);
CREATE INDEX l1_txs_block              ON l1_txs (block_slot, block_tx_index);                      -- placement, rollback
-- out_ref = l1_outputs PK; tx hash = l1_txs PK and l1_outputs PK prefix (the `*@txHash` query).
```

Live-at-slot `s` (the one predicate every "view at point" uses):
```sql
(o.created_slot IS NULL OR o.created_slot <= s) AND (o.spent_slot IS NULL OR o.spent_slot > s)
```
Same-block granularity (block-stage.ts:57-82) additionally compares `created_tx_index` and the spending tx's `block_tx_index`; that is done in the block stage, not the index.

Notes:
- Assets: jsonb keeps the exact-equality comparisons simple; `l1_output_assets` exists only because 13 `utxosAtWithUnit` sites plus `utxosAtWithPolicy` need an index. Dropping the jsonb and reassembling from the side table is equally valid.
- `address_bech32` is redundant with `address`. Keep it only if profiling shows re-encoding matters. It is a convenience column, not a fact.
- No quarantine column. Raw ledger facts cannot be malformed. Datum decode failure is a projection-level (kind D) concern.

---------------------------------------------------------------------------

## C. Rollback to point p = (p_slot, p_hash) and invariants

```sql
BEGIN;
-- Precondition: p is on our chain (the follower's intersection result).
SELECT 1 FROM l1_blocks WHERE slot = :p_slot AND hash = :p_hash FOR UPDATE;  -- must return 1 row, else abort

-- 1. Un-spend outputs spent after p (must run before the tx delete if FKs are not SET NULL).
UPDATE l1_outputs SET spent_slot = NULL, spent_tx = NULL WHERE spent_slot > :p_slot;

-- 2. Delete outputs created after p (bootstrap rows have created_slot NULL, never touched).
DELETE FROM l1_outputs WHERE created_slot > :p_slot;           -- cascades l1_output_assets

-- 3. Delete txs and blocks after p.
DELETE FROM l1_txs    WHERE block_slot > :p_slot;              -- cascades l1_tx_mint_policies
DELETE FROM l1_blocks WHERE slot > :p_slot;

-- 4. Move the cursor.
UPDATE l1_follower_cursor SET slot = :p_slot, hash = :p_hash;
COMMIT;
```
A rollback below `origin_slot` (the bootstrap point P) cannot be served. The follower must refuse it and trigger a re-bootstrap. With P at or below k-deep this is the "beyond k" case.

Invariant checks (each query must return zero rows):
```sql
-- I1 no output spent before (or in a block before) it was created
SELECT * FROM l1_outputs WHERE spent_slot IS NOT NULL AND created_slot IS NOT NULL AND spent_slot < created_slot;
-- I1b same-block: spending tx index must be after creating tx index
SELECT o.* FROM l1_outputs o JOIN l1_txs t ON t.tx_hash = o.spent_tx
 WHERE o.spent_slot = o.created_slot AND t.block_tx_index <= o.created_tx_index;
-- I2 every spent_tx exists in l1_txs and sits in spent_slot
SELECT o.* FROM l1_outputs o LEFT JOIN l1_txs t ON t.tx_hash = o.spent_tx
 WHERE o.spent_tx IS NOT NULL AND (t.tx_hash IS NULL OR t.block_slot <> o.spent_slot);
-- I3 spent_tx actually consumes the outref in the phase it ran
SELECT o.* FROM l1_outputs o JOIN l1_txs t ON t.tx_hash = o.spent_tx
 WHERE NOT ((t.is_valid AND (o.tx_hash || int2send(o.output_index::int2)) = ANY (t.inputs))
         OR (NOT t.is_valid AND (o.tx_hash || int2send(o.output_index::int2)) = ANY (t.collaterals)));
-- I4 every non-bootstrap output has its creating tx, and valid-ness matches index range
SELECT o.* FROM l1_outputs o LEFT JOIN l1_txs t ON t.tx_hash = o.tx_hash
 WHERE o.created_slot IS NOT NULL AND (t.tx_hash IS NULL OR t.block_slot <> o.created_slot
   OR (t.is_valid     AND o.output_index >= t.output_count)
   OR (NOT t.is_valid AND NOT (t.has_collateral_return AND o.output_index = t.output_count)));
-- I5 nothing above the cursor, blocks form a chain
SELECT * FROM l1_blocks b WHERE b.slot > (SELECT slot FROM l1_follower_cursor);
SELECT b.* FROM l1_blocks b LEFT JOIN l1_blocks pb ON pb.hash = b.parent_hash
 WHERE b.slot > (SELECT origin_slot FROM l1_follower_cursor) AND (pb.hash IS NULL OR pb.height + 1 <> b.height);
-- I6 bootstrap rows are never created after origin
SELECT * FROM l1_outputs WHERE created_slot IS NULL AND bootstrap_slot <> (SELECT origin_slot FROM l1_follower_cursor);
```
(The `int2send` concatenation assumes the 34-byte outref encoding in B. A composite type array is an equivalent choice.)

---------------------------------------------------------------------------

## D. Full body / witnesses vs decoded projection

Consumers needing raw bytes:
- Raw tx BODY CBOR is needed by exactly one path: creating-body verification for untracked historical references in list replay. That path covers l1-event-history-reference.ts:52-87 (hash check, size bound, CML round-trip) and l1-event-history-list-replay.ts:127-148, where the receipt carries the `creatingBodies` bytes. Its fetcher is event-history-owner.ts:451-489 / transport.ts:216-254.
- Those bodies belong to untracked txs (kind C), so the store never holds them in l1_txs anyway.
- No consumer reads the witness set. Redeemers are read as {purpose, index, cbor} only. Nothing reads vkey witnesses, witness datums or witness scripts.

Consumers satisfied by the decoded projection:
- Everything else: the transaction decoder (A.2), transitions, the observer and carriage.
- Carriage deliberately parses Ogmios JSON because `--include-transaction-cbor` cannot be required of an operator endpoint (l1-tx-order-carriage.ts:700-710). scripts/run-ogmios.sh does pass the flag.

Recommendation: store decoded columns only in l1_txs, and keep raw bytes in a separate kind-C content table keyed by tx hash:
- `l1_tx_bodies(tx_hash PK, body bytea)`. It is insert-only and never rolled back, because it is content-addressed and verified by hash, not a chain fact.
- It is populated only for the bodies the replay receipt needs.
- Storing raw CBOR for every tracked tx would make the follower depend on `--include-transaction-cbor`, which the repo explicitly refuses to require.

Size per tracked tx:

| representation | size |
|---|---|
| raw tx CBOR | at most 16,384 B (maxTxSize, midgard-core/src/consensus-profile.ts:95); protocol txs here are typically 1-8 KiB, dominated by redeemers |
| decoded l1_txs row | about 0.3-0.6 KiB fixed (hash, slot, 3 outref arrays of typically 1-10 x 34 B, mint/withdrawal jsonb), plus the redeemer cbor verbatim |

The redeemer cbor is typically 0.1-3 KiB, and the tx-order carriage redeemer can approach the tx size. The decoded form therefore saves roughly 50-80% on script-heavy txs and more on simple txs.

---------------------------------------------------------------------------

## E. Tracked address set and l1_txs qualification rule

Tracked addresses (config/deployment-derived, fixed at bootstrap; changes need re-bootstrap):
1. Hub oracle address (l1-event-history-source.ts:311-316)
2. Deposit list address; withdrawal list address; deposit retention and withdrawal retention addresses (same, 311-316)
3. Tx-order address (SDK tx-order.ts:277 via fetch-and-insert-tx-order-utxos.ts:85-106)
4. State-queue address (23 node read sites)
5. Scheduler; registered / active / retired operator lists (5, 5, 5, 2 sites)
6. Correction-lock address (5 sites; observer :775-931)
7. Settlement (3), reserve (3), payout (4, reserve-payout.ts requiredOutputIndexes)
8. Fraud-proof address (attestation-timeout-correction.ts:284; observer :933-977 fraud-proof reference)
9. DA params governor and DA attestation addresses (da-attestation.ts:120, 208)
10. stateQueueAuthValidator address, when distinct from the state-queue address (commit-submission.ts:576, merge.ts:333, confirm-block-commitments.ts:133, 145)
11. Reference-script publication address(es) (reference-scripts.ts, reference-publication.ts:211)
12. Own operator wallet address(es) (event-history-submission.ts, operator-wallet-view.ts:55, funding-preflight.ts:99, reference-publication.ts:557)
13. The CEK program material PAYMENT CREDENTIAL (fetch-and-insert-tx-order-utxos.ts:558-570). This is a credential match, not an address: any stake part qualifies.
14. zeroInput / list always-succeeds addresses only where a node path reads them (4 sites, always-succeeds testing only). Exclude them for public deploys.

The plan's list is items 1-6, 8, 9, 11 and 12. Items 7, 10, 13 (and 14 in testing) are additions found by the read inventory.

Output qualification: an output gets an l1_outputs row iff its address is in the set, or its payment credential equals a tracked credential (item 13).

Tx qualification: a tx (valid or failed) gets an l1_txs row iff at least one of these holds:
- (a) it creates a qualifying output. For a failed tx, only the collateral return counts.
- (b) it spends (inputs if valid, collaterals if failed) an outref present in l1_outputs.
- (c) it mints or burns under a tracked policy: hub, list, state-queue, correction-lock, fraud-proof, tx-order and DA-attestation policies. Activation (activation.ts:60-82) and observer (:1041-1071) key on these.
- (d) it references (reference input) an outref present in l1_outputs.

Rules (b) and (d) are decidable at follow time without extra I/O, because the referenced or spent outref is either already in l1_outputs (tracked), or created earlier in the same block (check the in-memory block stage first). A ref or input absent from both is untracked by the complete-scope argument, and the tx does not qualify on that account.

Rule (d) is stricter than Kupo, which never records reference-only txs. It is needed so the observer and carriage see txs that only reference tracked state (for example commit txs referencing the hub or scheduler). Drop (d) only if the owner rules those txs out of scope.

---------------------------------------------------------------------------

## F. Not servable by a tracked-address store (kind C or LSQ)

1. **Carriage reference-input datums** (l1-tx-order-carriage.ts:1063-1110; entry fetch-and-insert-tx-order-utxos.ts:344-350).
   - The refs sit at the order creator's wallet, which is untracked, and may already be spent.
   - Options: a by-hash creating-body fetch of each ref's creating tx (kind C, verified by hash), or an LSQ `utxo {outputReferences}` at the order's parent point while within k.
2. **Creating tx and mint redeemer of a tx order created before the store origin** (l1-tx-order-carriage.ts:1132-1152).
   - After origin, this is `T WHERE tx_hash = O.created_tx`. Before origin there is no T row, so it is kind C (body by hash; the redeemer lives in the witness set, so this needs the FULL tx CBOR, not only the body).
3. **Creating bodies for historical untracked references in list replay** (l1-event-history-list-replay.ts:127-148; l1-event-history-reference.ts:52-87; services/event-history-owner.ts:451-489, 692, 754-771; l1-event-history-transport.ts:216-254). Kind C by design.
4. **Activation tx location when activation predates the store origin** (l1-event-history-transport.ts:261-290, called at services/event-history-owner.ts:880; activation.ts:60-82). Kind C, or order bootstrap so that origin is at or before activation.
5. **CEK material bootstrap by payment credential** (fetch-and-insert-tx-order-utxos.ts:558-570).
   - LSQ `queryLedgerState/utxo` filters only by address or outref, never by credential. The bootstrap snapshot therefore cannot enumerate these outputs unless the material address (with its stake part) is pinned in config.
   - After origin the follower serves the credential index.
6. **Correction observer resolution of arbitrary spent and reference inputs** (services/state-queue-correction-observer.ts:1000-1020, via :833-874).
   - With a complete tracked scope, "not in l1_outputs" soundly means "not a correction lock / fraud proof" (both addresses are tracked).
   - The current code throws on a missing Kupo match, so this is a semantic change rather than a data gap.
   - If the owner wants the old "exactly one match" semantics, untracked inputs become kind C (UTxO content by outref, verified against the creating body).
7. **Datum-hash preimages** (Lucid getDatum): not a ledger fact. No current node use was found. If ever needed, it is kind C (verified by blake2b-256).
8. **Tip height and protocol parameters, getRewardAccount, submitTx** (l1-event-history-chain.ts:50-77; services/native-ledger.ts:66-85; services/lucid.ts:110). These are LSQ or submission calls and stay outside the store.
9. **Pre-origin history in general**: the bootstrap snapshot (l1-ledger-snapshot.ts:28-32) has no creation slot, spending history, bodies or redeemers.
   - Proposed resolution: run list replay from activation to P (kind-C bodies) and join it to the snapshot at P.
   - Snapshot rows not created within the replay window are marked pre-activation origin (`created_slot NULL`, `bootstrap_slot = P`).

---------------------------------------------------------------------------

## G. Open owner questions (only where designs differ materially)

1. **Raw tx CBOR.**
   - Option (a): make `--include-transaction-cbor` a hard operator requirement and store raw CBOR per tracked tx (up to 16 KiB each). This enables in-store verification and removes most kind-C body fetches for post-origin txs.
   - Option (b): keep decoded columns only (this draft) plus a by-hash kind-C body cache. This honours l1-tx-order-carriage.ts:700-710.
2. **Carriage ref-input resolution (F.1).** The choices:
   - LSQ outref query at the order's parent point: cheap, but only works within k, so a deep catch-up fails.
   - Kind-C creating-body fetch: needs a by-hash source other than Kupo, such as a peer or archive.
   - Tracking the ref-input outrefs dynamically as they appear in order redeemers: this changes the complete-scope argument.
3. **Spent-row retention / GC horizon.**
   - The observer's `*@txHash` reads (:876-931, :1192-1322), canonicalDepth (:1435-1486) and the event-key never-reuse rule (provenance.ts:266-268) all read spent rows.
   - The choice is keep-forever versus GC past a horizon, with never-reuse moved into a kind-D key-set projection. The memory rule "never unbounded retention" argues for the second, but it needs a stated horizon, for example max(k, observer window).
4. **CEK material by payment credential at bootstrap (F.5).** The choice:
   - pin the enterprise address in config so LSQ can query it,
   - allow a one-time full-UTxO LSQ scan, or
   - require bootstrap at or before material publication.
5. **Correction-observer complete-scope semantics (F.6).** Should "absent from the tracked store" be accepted as "not a lock / not a proof" (no kind C, simpler), or should unresolved untracked inputs remain a hard error resolved via kind C (preserves current fail-closed behaviour)?

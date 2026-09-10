import * as SDK from "@al-ft/midgard-sdk";
import {
  CML,
  coreToTxOutput,
  Data,
  getAddressDetails,
} from "@lucid-evolution/lucid";
import { beforeAll, describe, expect, it, vi } from "vitest";

import { canonicalBlockEvidenceFromVerifiedPayload } from "../src/evidence/canonical-block-evidence.js";
import {
  captureTransitionTraceL1Events,
  readFreshTransitionTraceL1Events,
  requireTransitionTraceL1Events,
} from "../src/transition-trace/l1-events.js";
import { computeTransitionTraceL1EventEvidenceDigest } from "../src/transition-trace/replay-authority.js";
import {
  admitCompleteCanonicalReplayPredecessor,
  admitValidationTraceReplayContext,
  VALIDATION_TRACE_DISPUTE_COMPLETE_CANONICAL_REPLAY,
} from "../src/workflow/complete-replay.js";
import * as rawSnapshot from "../src/workflow/raw-l1-snapshot.js";
import {
  computeFraudProofRawL1PointId,
  computeFraudProofRawL1RollbackCursor,
  FRAUD_PROOF_RAW_L1_SNAPSHOT_AUTHORITY,
  type FraudProofRawL1Snapshot,
  type FraudProofRawL1SnapshotAuthority,
} from "../src/workflow/raw-l1-snapshot.js";
import {
  FRAUD_PROOF_RELEASE_FINALITY_AUTHORITY,
  FRAUD_PROOF_RELEASE_FINALITY_POLICY_SCHEMA_VERSION,
} from "../src/workflow/release-finality-policy.js";
import { authenticatedHeaderObservation } from "./helpers/canonical-block-evidence-fixture.js";
import {
  buildRetainedPlutusIdentityFixture,
  captureRetainedPlutusIdentityOrigins,
  classifyRetainedReasonFixture,
} from "./support/retained-reason-classifier.js";

let seed: FraudProofRawL1Snapshot;
let retained: Awaited<ReturnType<typeof buildRetainedPlutusIdentityFixture>>;
beforeAll(async () => {
  retained = await buildRetainedPlutusIdentityFixture(
    { verdict: "accepted" },
    { sourceKind: "forced" },
  );
  seed = requireTransitionTraceL1Events(
    await captureRetainedPlutusIdentityOrigins(retained),
  ).snapshot;
});

const capture = (snapshot: FraudProofRawL1Snapshot) => {
  const hubScope = snapshot.scopes.find(({ role }) => role === "hub_oracle")!;
  const authority: FraudProofRawL1SnapshotAuthority = {
    authorityVersion: FRAUD_PROOF_RAW_L1_SNAPSHOT_AUTHORITY,
    capture: async (request) => ({
      ...snapshot,
      scopes: request.scopes.map((scope) => ({
        ...scope,
        utxos: snapshot.scopes.find(({ role }) => role === scope.role)!.utxos,
      })),
      historyUnits: request.historyUnits,
      history: request.historyUnits.map(
        (unit) => snapshot.history.find((entry) => entry.unit === unit)!,
      ),
    }),
  };
  const binding = {
    network: "Preprod",
    releaseFinality: {
      schemaVersion: FRAUD_PROOF_RELEASE_FINALITY_POLICY_SCHEMA_VERSION,
      deploymentIdentityDigest: snapshot.deploymentIdentityDigest,
      blueprintHash: snapshot.blueprintHash,
      policyDigest: snapshot.finalityPolicyDigest,
      policy: {
        confirmationDepth: 30,
        automaticRecoveryMaxDepth: 2160,
        deepRollbackPolicy: "automated_rewind_replay_incident-v1",
      },
    },
    resolvedContracts: {
      hubOraclePolicyId: getAddressDetails(hubScope.address).paymentCredential!
        .hash,
    },
    definition: { headerHash: snapshot.headerHash },
  } as Parameters<typeof captureTransitionTraceL1Events>[0]["binding"];
  return captureTransitionTraceL1Events({ binding, authority });
};

const advance = (
  snapshot: FraudProofRawL1Snapshot,
): FraudProofRawL1Snapshot => {
  const next = (point: typeof snapshot.cursor.point, hash: string) => {
    const value = {
      slot: (BigInt(point.slot) + 1n).toString(),
      blockNo: (BigInt(point.blockNo) + 1n).toString(),
      blockHash: hash.repeat(32),
    };
    return { ...value, pointId: computeFraudProofRawL1PointId(value) };
  };
  const point = next(snapshot.cursor.point, "81");
  const tip = next(snapshot.cursor.tip, "82");
  return {
    ...snapshot,
    provenance: {
      ...snapshot.provenance,
      kupoCheckpoint: point,
      ogmiosTip: tip,
    },
    cursor: {
      ...snapshot.cursor,
      point,
      tip,
      rollbackCursor: computeFraudProofRawL1RollbackCursor({
        ...snapshot,
        sourceId: snapshot.provenance.sourceId,
        pointId: point.pointId,
      }),
    },
    history: snapshot.history.map((entry) => ({
      ...entry,
      completeThroughPointId: point.pointId,
    })),
    transactions: snapshot.transactions.map((entry) => ({
      ...entry,
      confirmationDepth: entry.confirmationDepth + 1,
    })),
  };
};

const replaceBody = (
  snapshot: FraudProofRawL1Snapshot,
  update: (body: CML.TransactionBody) => CML.TransactionBody,
): FraudProofRawL1Snapshot => {
  const transaction = snapshot.transactions[0]!;
  const body = update(CML.TransactionBody.from_cbor_hex(transaction.bodyCbor));
  const txHash = CML.hash_transaction(body).to_hex();
  return {
    ...snapshot,
    scopes: snapshot.scopes.map((scope) => ({
      ...scope,
      utxos: scope.utxos.map((entry) => {
        const [oldHash, index] = entry.outRef.split("#");
        if (oldHash !== transaction.txHash) return entry;
        const outputCbor = body
          .outputs()
          .get(Number(index))
          .to_canonical_cbor_hex();
        return {
          ...entry,
          outRef: `${txHash}#${index}`,
          outputCbor,
          datumCbor:
            coreToTxOutput(CML.TransactionOutput.from_cbor_hex(outputCbor))
              .datum ?? null,
        };
      }),
    })),
    history: snapshot.history.map((entry) => ({
      ...entry,
      transactionHashes: entry.transactionHashes.map((hash) =>
        hash === transaction.txHash ? txHash : hash,
      ),
    })),
    transactions: [
      { ...transaction, txHash, bodyCbor: body.to_cbor_hex() },
      ...snapshot.transactions.slice(1),
    ],
  };
};

const changeDatum = (
  snapshot: FraudProofRawL1Snapshot,
  index: number,
  datum: string,
) =>
  replaceBody(snapshot, (body) => {
    const outputs = body.outputs();
    const next = CML.TransactionOutputList.new();
    for (let i = 0; i < outputs.len(); i += 1) {
      const output = outputs.get(i);
      next.add(
        i === index
          ? CML.TransactionOutput.new(
              output.address(),
              output.amount(),
              CML.DatumOption.new_datum(CML.PlutusData.from_cbor_hex(datum)),
            )
          : output,
      );
    }
    const updated = CML.TransactionBody.new(body.inputs(), next, body.fee());
    updated.set_mint(body.mint()!);
    return updated;
  });

const withLaterEvent = (
  snapshot: FraudProofRawL1Snapshot,
  inclusionTime = retained.block.header.endTime + 1n,
): FraudProofRawL1Snapshot => {
  const scope = snapshot.scopes.find(
    (entry) => entry.role === "forced_transaction_event",
  )!;
  const original = scope.utxos[0]!;
  const datum = Data.from(original.datumCbor!, SDK.TxOrderDatum);
  const updated = {
    ...datum,
    event: {
      ...datum.event,
      id: { transactionId: "91".repeat(32), outputIndex: 0n },
    },
    inclusion_time: inclusionTime,
  };
  const policy = getAddressDetails(scope.address).paymentCredential!.hash;
  const unit = policy + "02";
  const assets = CML.MultiAsset.new();
  assets.set(CML.ScriptHash.from_hex(policy), CML.AssetName.from_hex("02"), 1n);
  const output = CML.TransactionOutput.new(
    CML.Address.from_bech32(scope.address),
    CML.Value.new(3_000_000n, assets),
    CML.DatumOption.new_datum(
      CML.PlutusData.from_cbor_hex(Data.to(updated, SDK.TxOrderDatum)),
    ),
  );
  const outputs = CML.TransactionOutputList.new();
  outputs.add(output);
  const inputs = CML.TransactionInputList.new();
  inputs.add(
    CML.TransactionInput.new(CML.TransactionHash.from_hex("91".repeat(32)), 0n),
  );
  const body = CML.TransactionBody.new(inputs, outputs, 170_000n);
  const mint = CML.Mint.new();
  mint.set(CML.ScriptHash.from_hex(policy), CML.AssetName.from_hex("02"), 1n);
  body.set_mint(mint);
  const txHash = CML.hash_transaction(body).to_hex();
  const outputCbor = output.to_canonical_cbor_hex();
  const raw = {
    outRef: `${txHash}#0`,
    outputCbor,
    datumCbor: coreToTxOutput(CML.TransactionOutput.from_cbor_hex(outputCbor))
      .datum!,
    referenceScriptCbor: null,
  };
  return {
    ...snapshot,
    scopes: snapshot.scopes.map((entry) =>
      entry.role === scope.role
        ? { ...entry, utxos: [...entry.utxos, raw] }
        : entry,
    ),
    historyUnits: [...snapshot.historyUnits, unit],
    history: [
      ...snapshot.history,
      {
        unit,
        fromGenesis: true,
        completeThroughPointId: snapshot.cursor.point.pointId,
        transactionHashes: [txHash],
      },
    ],
    transactions: [
      ...snapshot.transactions,
      {
        ...snapshot.transactions[0]!,
        txHash,
        bodyCbor: body.to_cbor_hex(),
        resolvedInputs: [
          {
            ...snapshot.transactions[0]!.resolvedInputs[0]!,
            outRef: "91".repeat(32) + "#0",
          },
        ],
      },
    ],
  };
};

describe("transition trace immutable L1 evidence", () => {
  it("keeps an old header decision stable after a later unrelated event while binding relevant origins", async () => {
    const daProvenance = {
      trustClass: "public_or_permissionless_da",
      sourceId: "retained-fixture/emulator",
      grade: "security",
    } as const;
    const observation = authenticatedHeaderObservation(retained.block);
    const evidence = await canonicalBlockEvidenceFromVerifiedPayload({
      observation,
      payloadEnvelopeCbor: retained.block.payloadEnvelopeCbor,
      daProvenance,
      minimumConfirmationDepth: 30,
    });
    const predecessor = await admitCompleteCanonicalReplayPredecessor({
      value: {
        observation: authenticatedHeaderObservation(retained.predecessor),
        payloadEnvelopeCborHex:
          retained.predecessor.payloadEnvelopeCbor.toString("hex"),
        daProvenance,
      },
      currentEvidence: evidence,
      minimumConfirmationDepth: 30,
    });
    const first = await capture(seed);
    const later = await capture(withLaterEvent(advance(seed)));
    expect(later.snapshotDigest).not.toBe(first.snapshotDigest);
    expect(later.evidenceDigest).not.toBe(first.evidenceDigest);
    expect(
      computeTransitionTraceL1EventEvidenceDigest({
        evidence,
        l1Events: later,
      }),
    ).toBe(
      computeTransitionTraceL1EventEvidenceDigest({
        evidence,
        l1Events: first,
      }),
    );
    const classify = async (transitionTraceEvents: typeof first) => {
      const validationTraceReplay = await admitValidationTraceReplayContext({
        evidence,
        predecessor,
        transitionTraceEvents,
      });
      return (
        await classifyRetainedReasonFixture({
          observation,
          payloadEnvelopeCbor: retained.block.payloadEnvelopeCbor,
          deploymentFingerprint: seed.deploymentIdentityDigest,
          releaseFinalityAuthority: {
            authorityVersion: FRAUD_PROOF_RELEASE_FINALITY_AUTHORITY,
            verifyForWorkflow: async () => ({
              schemaVersion: FRAUD_PROOF_RELEASE_FINALITY_POLICY_SCHEMA_VERSION,
              deploymentIdentityDigest: seed.deploymentIdentityDigest,
              blueprintHash: seed.blueprintHash,
              policyDigest: seed.finalityPolicyDigest,
              policy: {
                confirmationDepth: 30,
                automaticRecoveryMaxDepth: 2160,
                deepRollbackPolicy: "automated_rewind_replay_incident-v1",
              },
            }),
          },
          replayer: VALIDATION_TRACE_DISPUTE_COMPLETE_CANONICAL_REPLAY,
          replayContext: {
            predecessor,
            transitionTraceEvents,
            validationTraceReplay,
          },
        })
      ).decision;
    };
    expect(await classify(later)).toEqual(await classify(first));
    const due = await capture(
      withLaterEvent(advance(seed), retained.block.header.endTime),
    );
    expect(
      computeTransitionTraceL1EventEvidenceDigest({ evidence, l1Events: due }),
    ).not.toBe(
      computeTransitionTraceL1EventEvidenceDigest({
        evidence,
        l1Events: first,
      }),
    );
    const changed = await capture(
      replaceBody(seed, (body) => {
        const next = CML.TransactionBody.new(
          body.inputs(),
          body.outputs(),
          body.fee() + 1n,
        );
        next.set_mint(body.mint()!);
        return next;
      }),
    );
    expect(
      computeTransitionTraceL1EventEvidenceDigest({
        evidence,
        l1Events: changed,
      }),
    ).not.toBe(
      computeTransitionTraceL1EventEvidenceDigest({
        evidence,
        l1Events: first,
      }),
    );
    const missing = await capture({
      ...seed,
      scopes: seed.scopes.map((entry) =>
        entry.role === "forced_transaction_event"
          ? { ...entry, utxos: [] }
          : entry,
      ),
    });
    expect(() =>
      computeTransitionTraceL1EventEvidenceDigest({
        evidence,
        l1Events: missing,
      }),
    ).toThrow("coverage for a committed source");
  });

  it("owns immutable admitted evidence and recomputes its digest only when the projection changes", async () => {
    const input = structuredClone(seed);
    const handle = await capture(input);
    const view = readFreshTransitionTraceL1Events(handle);
    const body = view.snapshot.transactions[0]!.bodyCbor;
    expect(Reflect.set(input.transactions[0]!, "bodyCbor", "00")).toBe(true);
    expect(view.snapshot.transactions[0]!.bodyCbor).toBe(body);
    expect(Reflect.set(view.snapshot.transactions[0]!, "bodyCbor", "00")).toBe(
      false,
    );
    expect(Reflect.set(view.events[0]!.utxo.assets, "lovelace", 0n)).toBe(
      false,
    );
    expect(readFreshTransitionTraceL1Events(handle)).toBe(view);
    expect(() => readFreshTransitionTraceL1Events({ ...handle })).toThrow(
      "freshly admitted",
    );
    const evidence = await canonicalBlockEvidenceFromVerifiedPayload({
      observation: authenticatedHeaderObservation(retained.block),
      payloadEnvelopeCbor: retained.block.payloadEnvelopeCbor,
      daProvenance: {
        trustClass: "public_or_permissionless_da",
        sourceId: "retained-fixture/emulator",
        grade: "security",
      },
      minimumConfirmationDepth: 30,
    });
    const compute = vi.spyOn(
      rawSnapshot,
      "computeFraudProofRawL1SnapshotEvidenceDigest",
    );
    try {
      const first = computeTransitionTraceL1EventEvidenceDigest({
        evidence,
        l1Events: handle,
      });
      expect(
        computeTransitionTraceL1EventEvidenceDigest({
          evidence,
          l1Events: handle,
        }),
      ).toBe(first);
      expect(compute).toHaveBeenCalledTimes(1);
      const changed = { ...evidence, payloadSha256: "ff".repeat(32) };
      expect(
        computeTransitionTraceL1EventEvidenceDigest({
          evidence: changed,
          l1Events: handle,
        }),
      ).not.toBe(first);
      expect(compute).toHaveBeenCalledTimes(2);
    } finally {
      compute.mockRestore();
    }
  });

  it("keeps evidence identity stable as admitted snapshot points advance", async () => {
    const first = await capture(seed);
    const later = await capture(advance(seed));
    expect(later.snapshotDigest).not.toBe(first.snapshotDigest);
    expect(later.evidenceDigest).toBe(first.evidenceDigest);
    expect(requireTransitionTraceL1Events(later).events).toHaveLength(1);
    expect(() => requireTransitionTraceL1Events({ ...later })).toThrow(
      "freshly admitted",
    );
  });

  it.each([
    "body",
    "witness",
    "input",
    "inclusion",
    "membership",
    "event",
    "hub",
  ] as const)(
    "binds changed %s evidence even when the event verdict need not change",
    async (kind) => {
      let changed = structuredClone(seed);
      const firstTx = changed.transactions[0]!;
      if (kind === "body")
        changed = replaceBody(changed, (body) => {
          const updated = CML.TransactionBody.new(
            body.inputs(),
            body.outputs(),
            body.fee() + 1n,
          );
          updated.set_mint(body.mint()!);
          return updated;
        });
      if (kind === "witness")
        changed = {
          ...changed,
          transactions: [{ ...firstTx, witnessSetCbor: "a1048100" }],
        };
      if (kind === "input") {
        const input = firstTx.resolvedInputs[0]!;
        const output = CML.TransactionOutput.from_cbor_hex(input.outputCbor);
        const outputCbor = CML.TransactionOutput.new(
          output.address(),
          CML.Value.from_coin(11_000_000n),
        ).to_canonical_cbor_hex();
        changed = {
          ...changed,
          transactions: [
            { ...firstTx, resolvedInputs: [{ ...input, outputCbor }] },
          ],
        };
      }
      if (kind === "inclusion") {
        const point = { ...firstTx.inclusionPoint, blockHash: "83".repeat(32) };
        changed = {
          ...changed,
          transactions: [
            {
              ...firstTx,
              inclusionPoint: {
                ...point,
                pointId: computeFraudProofRawL1PointId(point),
              },
            },
          ],
        };
      }
      if (kind === "membership")
        changed = {
          ...changed,
          scopes: changed.scopes.map((scope) =>
            scope.role === "forced_transaction_event"
              ? { ...scope, utxos: [] }
              : scope,
          ),
        };
      if (kind === "event") {
        const raw = changed.scopes.find(
          ({ role }) => role === "forced_transaction_event",
        )!.utxos[0]!;
        const datum = Data.from(raw.datumCbor!, SDK.TxOrderDatum);
        changed = changeDatum(
          changed,
          Number(raw.outRef.split("#")[1]),
          Data.to(
            { ...datum, inclusion_time: datum.inclusion_time + 1n },
            SDK.TxOrderDatum,
          ),
        );
      }
      if (kind === "hub") {
        const raw = changed.scopes.find(({ role }) => role === "hub_oracle")!
          .utxos[0]!;
        const datum = Data.from(raw.datumCbor!, SDK.HubOracleDatum);
        changed = changeDatum(
          changed,
          Number(raw.outRef.split("#")[1]),
          Data.to(
            { ...datum, reserve_observer: "85".repeat(28) },
            SDK.HubOracleDatum,
          ),
        );
      }
      expect((await capture(changed)).evidenceDigest).not.toBe(
        (await capture(seed)).evidenceDigest,
      );
    },
  );

  it("still refuses incomplete history, inconsistent redeemers and insufficient finality", async () => {
    await expect(
      capture({
        ...seed,
        history: seed.history.map((entry) => ({
          ...entry,
          transactionHashes: [],
        })),
      }),
    ).rejects.toThrow("incomplete");
    await expect(
      capture({
        ...seed,
        transactions: seed.transactions.map((entry) => ({
          ...entry,
          redeemersCbor: "80",
        })),
      }),
    ).rejects.toThrow("differs from the witness set");
    await expect(
      capture({ ...seed, cursor: { ...seed.cursor, confirmationDepth: 29 } }),
    ).rejects.toThrow("below release finality");
  });
});

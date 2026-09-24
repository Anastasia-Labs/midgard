import * as SDK from "@al-ft/midgard-sdk";
import {
  CML,
  coreToTxOutput,
  Data,
  getAddressDetails,
} from "@lucid-evolution/lucid";
import { Effect } from "effect";
import { beforeAll, describe, expect, it, vi } from "vitest";

import { canonicalBlockEvidenceFromVerifiedPayload } from "../src/evidence/canonical-block-evidence.js";
import * as transitionDetection from "../src/transition-trace/detect.js";
import {
  captureTransitionTraceL1Events,
  createTransitionTraceEventAuthority,
  readFreshTransitionTraceL1Events,
  requireTransitionTraceL1Events,
} from "../src/transition-trace/l1-events.js";
import {
  computeTransitionTraceL1EventEvidenceDigest,
  replayTransitionTraceFromRetainedHistory,
} from "../src/transition-trace/replay-authority.js";
import { VALIDATION_TRACE_DISPUTE_COMPLETE_CANONICAL_REPLAY } from "../src/workflow/complete-replay.js";
import * as historicalCorpus from "../src/workflow/historical-native-script-corpus.js";
import { LocalKupmiosCheckpointChangedError } from "../src/workflow/local-kupmios-raw-l1-authority.js";
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
import { TRANSITION_HISTORY_FIXTURE_PARAMETERS } from "./helpers/transition-history-fixture.js";
import {
  buildRetainedPlutusIdentityFixture,
  captureRetainedPlutusIdentityOrigins,
  classifyRetainedReasonFixture,
} from "./support/retained-reason-classifier.js";

const transport = vi.hoisted(() => ({ raw: undefined as unknown }));
vi.mock("../src/workflow/family-l1-observation.js", async (load) => {
  const actual =
    await load<typeof import("../src/workflow/family-l1-observation.js")>();
  return {
    ...actual,
    createFraudProofFamilyLocalKupmiosL1ObservationPort: () => ({
      rawL1: transport.raw,
    }),
  };
});

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

const captureInput = (snapshot: FraudProofRawL1Snapshot) => {
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
    deploymentFingerprint: snapshot.deploymentIdentityDigest,
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
      contracts: {
        transitionTrace: { history: TRANSITION_HISTORY_FIXTURE_PARAMETERS },
      },
      hubOraclePolicyId: getAddressDetails(hubScope.address).paymentCredential!
        .hash,
    },
    definition: { headerHash: snapshot.headerHash },
  } as Parameters<typeof captureTransitionTraceL1Events>[0]["binding"];
  return { binding, authority };
};

const capture = (snapshot: FraudProofRawL1Snapshot) =>
  captureTransitionTraceL1Events(captureInput(snapshot));

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
  assetName = "02",
  quantity = 1n,
): FraudProofRawL1Snapshot => {
  const scope = snapshot.scopes.find(
    (entry) => entry.role === "forced_transaction_event",
  )!;
  const original = scope.utxos[0]!;
  const datum = Data.from(original.datumCbor!, SDK.TxOrderDatum);
  const sourceTxHash =
    assetName === "02" ? "91".repeat(32) : assetName.repeat(32);
  const updated = {
    ...datum,
    event: {
      ...datum.event,
      id: { transactionId: sourceTxHash, outputIndex: 0n },
    },
    inclusion_time: inclusionTime,
  };
  const policy = getAddressDetails(scope.address).paymentCredential!.hash;
  const unit = policy + assetName;
  const assets = CML.MultiAsset.new();
  assets.set(
    CML.ScriptHash.from_hex(policy),
    CML.AssetName.from_hex(assetName),
    quantity,
  );
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
    CML.TransactionInput.new(CML.TransactionHash.from_hex(sourceTxHash), 0n),
  );
  const body = CML.TransactionBody.new(inputs, outputs, 170_000n);
  const mint = CML.Mint.new();
  mint.set(
    CML.ScriptHash.from_hex(policy),
    CML.AssetName.from_hex(assetName),
    quantity,
  );
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
            outRef: sourceTxHash + "#0",
          },
        ],
      },
    ],
  };
};

describe("transition trace immutable L1 evidence", () => {
  it("recaptures complete history when an authenticated event arrives after discovery", async () => {
    const later = withLaterEvent(advance(seed));
    const initial = captureInput(seed);
    const advanced = captureInput(later);
    let captureCount = 0;
    const captures = vi.fn(
      async (request: rawSnapshot.FraudProofRawL1SnapshotRequest) =>
        await (++captureCount <= 2 ? initial : advanced).authority.capture(
          request,
        ),
    );
    const handle = await captureTransitionTraceL1Events({
      binding: initial.binding,
      authority: { ...initial.authority, capture: captures },
    });
    const admitted = requireTransitionTraceL1Events(handle);
    expect(captures).toHaveBeenCalledTimes(4);
    expect(admitted.events).toHaveLength(2);
    expect(admitted.snapshot.historyUnits).toEqual(
      [...later.historyUnits].sort(),
    );
    expect(admitted.snapshot.history).toHaveLength(later.history.length);
    expect(admitted.snapshot.cursor).toEqual(later.cursor);
  });

  it("bounds repeated valid event growth and leaves classification pending", async () => {
    const snapshots = [seed, seed];
    let current = seed;
    for (const assetName of ["02", "03", "04"]) {
      current = withLaterEvent(advance(current), undefined, assetName);
      snapshots.push(current);
    }
    let captures = 0;
    const initial = captureInput(seed);
    await expect(
      captureTransitionTraceL1Events({
        binding: initial.binding,
        authority: {
          ...initial.authority,
          capture: async (request) => {
            const snapshot = snapshots[captures++];
            if (snapshot === undefined)
              throw new Error("capture bound exceeded");
            return await captureInput(snapshot).authority.capture(request);
          },
        },
      }),
    ).rejects.toBeInstanceOf(LocalKupmiosCheckpointChangedError);
    expect(captures).toBe(5);
  });

  it.each(["quantity", "datum"] as const)(
    "does not retry malformed %s as coverage growth",
    async (kind) => {
      let later = withLaterEvent(
        advance(seed),
        undefined,
        "02",
        kind === "quantity" ? 2n : 1n,
      );
      if (kind === "datum") {
        const original = seed.scopes.find(
          ({ role }) => role === "forced_transaction_event",
        )!.utxos[0]!;
        later = changeDatum(
          later,
          Number(original.outRef.split("#")[1]),
          Data.to(42n),
        );
      }
      let captures = 0;
      const initial = captureInput(seed);
      const result = captureTransitionTraceL1Events({
        binding: initial.binding,
        authority: {
          ...initial.authority,
          capture: async (request) =>
            await captureInput(
              ++captures <= 2 ? seed : later,
            ).authority.capture(request),
        },
      });
      await expect(result).rejects.toBeInstanceOf(Error);
      await expect(result).rejects.not.toBeInstanceOf(
        LocalKupmiosCheckpointChangedError,
      );
      expect(captures).toBe(3);
    },
  );

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
      const { binding, authority } = captureInput(
        requireTransitionTraceL1Events(transitionTraceEvents).snapshot,
      );
      transport.raw = authority;
      const transitionTraceEventAuthority = createTransitionTraceEventAuthority(
        {
          binding,
          source: {} as never,
        },
      );
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
          transitionTraceEventAuthority,
          predecessor: {
            observation: authenticatedHeaderObservation(retained.predecessor),
            payloadEnvelopeCbor: retained.predecessor.payloadEnvelopeCbor,
          },
        })
      ).decision;
    };
    const firstDecision = await classify(first);
    expect(firstDecision.decision).toBe("healthy");
    expect(await classify(later)).toEqual(firstDecision);
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

  it("accepts authenticated event history at inclusion depth", async () => {
    const tip = seed.cursor.point;
    const included = await capture({
      ...seed,
      provenance: { ...seed.provenance, ogmiosTip: tip },
      cursor: { ...seed.cursor, tip, confirmationDepth: 1 },
      transactions: seed.transactions.map((entry) => ({
        ...entry,
        confirmationDepth:
          Number(tip.blockNo) - Number(entry.inclusionPoint.blockNo) + 1,
      })),
    });
    expect(
      requireTransitionTraceL1Events(included).snapshot.cursor
        .confirmationDepth,
    ).toBe(1);
  });

  it("still refuses incomplete history, inconsistent redeemers and inconsistent depth", async () => {
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
    ).rejects.toThrow("confirmation depth disagrees");
  });
});

describe("immutable transition event decoding reuse", () => {
  it("reuses admitted event parsing while rechecking mutable header and source coverage", async () => {
    const evidence = await canonicalBlockEvidenceFromVerifiedPayload({
      observation: authenticatedHeaderObservation(retained.block),
      payloadEnvelopeCbor: retained.block.payloadEnvelopeCbor,
      daProvenance: {
        trustClass: "public_or_permissionless_da",
        sourceId: "retained-fixture/event-facts",
        grade: "security",
      },
      minimumConfirmationDepth: 30,
    });
    const handle = await capture(seed);
    const freshHandle = await capture(advance(seed));
    const decode = vi.spyOn(Data, "from");
    try {
      const digest = computeTransitionTraceL1EventEvidenceDigest({
        evidence,
        l1Events: handle,
      });
      const initialDecodes = decode.mock.calls.length;
      expect(initialDecodes).toBeGreaterThan(0);
      const started = performance.now();
      for (let index = 0; index < 100; index += 1)
        expect(
          computeTransitionTraceL1EventEvidenceDigest({
            evidence,
            l1Events: handle,
          }),
        ).toBe(digest);
      console.info(
        JSON.stringify({
          benchmark: "same-admitted-event-handle-100-digest-reads",
          elapsedMs: performance.now() - started,
          initialDecodes,
          repeatedDecodes: decode.mock.calls.length - initialDecodes,
        }),
      );
      expect(decode.mock.calls.length).toBe(initialDecodes);
      const changedHeader = {
        ...evidence.reconstruction.header,
        endTime: evidence.reconstruction.header.endTime + 1n,
      };
      expect(
        computeTransitionTraceL1EventEvidenceDigest({
          evidence: {
            ...evidence,
            reconstruction: {
              ...evidence.reconstruction,
              header: changedHeader,
            },
          },
          l1Events: handle,
        }),
      ).not.toBe(digest);
      const forced = evidence.reconstruction.sourceEvents.find(
        (source) => source.phase === "ForcedTransaction",
      );
      if (forced === undefined)
        throw new Error("fixture requires a forced source");
      const sources = new Map(
        evidence.reconstruction.sourceEventsByFingerprint,
      );
      sources.set("uncovered-forced-source", {
        ...forced,
        fingerprint: "uncovered-forced-source",
      });
      expect(() =>
        computeTransitionTraceL1EventEvidenceDigest({
          evidence: {
            ...evidence,
            reconstruction: {
              ...evidence.reconstruction,
              sourceEventsByFingerprint: sources,
            },
          },
          l1Events: handle,
        }),
      ).toThrow("lacks authenticated L1 coverage for a committed source");
      expect(() =>
        computeTransitionTraceL1EventEvidenceDigest({
          evidence,
          l1Events: { ...handle },
        }),
      ).toThrow("requires freshly admitted raw L1 events");
      expect(decode.mock.calls.length).toBe(initialDecodes);
      expect(
        computeTransitionTraceL1EventEvidenceDigest({
          evidence,
          l1Events: freshHandle,
        }),
      ).toBe(digest);
      expect(decode.mock.calls.length).toBeGreaterThan(initialDecodes);
    } finally {
      decode.mockRestore();
    }
  });
});

describe("immutable event parsing failure and contextual outputs", () => {
  it("does not cache failed forced transaction decoding or identity validation", async () => {
    const evidence = await canonicalBlockEvidenceFromVerifiedPayload({
      observation: authenticatedHeaderObservation(retained.block),
      payloadEnvelopeCbor: retained.block.payloadEnvelopeCbor,
      daProvenance: {
        trustClass: "public_or_permissionless_da",
        sourceId: "retained-fixture/event-facts-failure",
        grade: "security",
      },
      minimumConfirmationDepth: 30,
    });
    const raw = seed.scopes.find(
      (scope) => scope.role === "forced_transaction_event",
    )!.utxos[0]!;
    const index = Number(raw.outRef.split("#")[1]);
    const original = Data.from(raw.datumCbor!, SDK.TxOrderDatum);
    for (const transaction of [
      { ...original.event.tx, tx_id: "ff".repeat(32) },
      {
        ...original.event.tx,
        submitted_source: {
          ...original.event.tx.submitted_source,
          compact_cbor: "00",
        },
      },
    ]) {
      const malformed = await capture(
        changeDatum(
          seed,
          index,
          Data.to(
            {
              ...original,
              event: { ...original.event, tx: transaction },
            },
            SDK.TxOrderDatum,
          ),
        ),
      );
      const decode = vi.spyOn(Data, "from");
      try {
        for (let attempt = 0; attempt < 2; attempt += 1) {
          const prior = decode.mock.calls.length;
          expect(() =>
            computeTransitionTraceL1EventEvidenceDigest({
              evidence,
              l1Events: malformed,
            }),
          ).toThrow();
          expect(decode.mock.calls.length).toBeGreaterThan(prior);
        }
      } finally {
        decode.mockRestore();
      }
    }
    const valid = await capture(seed);
    expect(
      computeTransitionTraceL1EventEvidenceDigest({
        evidence,
        l1Events: valid,
      }),
    ).toMatch(/^[0-9a-f]{64}$/u);
  });

  it("detaches cached event IDs and keeps withdrawal timing identity independent of verdicts", async () => {
    const evidence = await canonicalBlockEvidenceFromVerifiedPayload({
      observation: authenticatedHeaderObservation(retained.block),
      payloadEnvelopeCbor: retained.block.payloadEnvelopeCbor,
      daProvenance: {
        trustClass: "public_or_permissionless_da",
        sourceId: "retained-fixture/event-facts-outputs",
        grade: "security",
      },
      minimumConfirmationDepth: 30,
    });
    const forcedScope = seed.scopes.find(
      (scope) => scope.role === "forced_transaction_event",
    )!;
    const withdrawalScope = seed.scopes.find(
      (scope) => scope.role === "withdrawal_event",
    )!;
    const original = Data.from(
      forcedScope.utxos[0]!.datumCbor!,
      SDK.TxOrderDatum,
    );
    if (original.refund_address.stakeCredential !== null)
      throw new Error("fixture requires an unstaked refund address");
    const refundAddress = {
      paymentCredential: original.refund_address.paymentCredential,
      stakeCredential: null,
    };
    const info: SDK.WithdrawalInfo = {
      body: {
        l2_outref: original.event.id,
        l2_owner: "aa".repeat(28),
        l2_value: new Map([["", new Map([["", 3_000_000n]])]]),
        l1_address: refundAddress,
        l1_datum: "NoDatum",
      },
      signature: ["", ""],
      validity: { SpentWithdrawalUtxo: { l2_tx_id: "ab".repeat(32) } },
    };
    const datum: SDK.WithdrawalOrderDatum = {
      event: { id: original.event.id, info },
      inclusion_time: evidence.header.endTime + 1n,
      witness: original.witness,
      refund_address: refundAddress,
      refund_datum: original.refund_datum,
    };
    const policy = getAddressDetails(withdrawalScope.address).paymentCredential!
      .hash;
    const oldUnit =
      getAddressDetails(forcedScope.address).paymentCredential!.hash + "01";
    const key = await Effect.runPromise(SDK.eventHistoryKey(datum.event.id));
    const unit = policy + key;
    const order: SDK.EventHistoryNode = {
      position: { Key: [key] },
      next: null,
      protected_until: 0n,
      payload: {
        Order: {
          facts: {
            event_id: datum.event.id,
            inclusion_time: datum.inclusion_time,
            structural_lovelace: 1_000_000n,
            structural_refund_key: "aa".repeat(28),
            location: {
              Inline: {
                payload: {
                  WithdrawalPayload: {
                    event: datum.event,
                    refund_address: datum.refund_address,
                    refund_datum: datum.refund_datum,
                  },
                },
              },
            },
          },
        },
      },
    };
    const changed = replaceBody(seed, (body) => {
      const outputs = CML.TransactionOutputList.new();
      outputs.add(body.outputs().get(0));
      const assets = CML.MultiAsset.new();
      assets.set(
        CML.ScriptHash.from_hex(policy),
        CML.AssetName.from_hex(key),
        1n,
      );
      outputs.add(
        CML.TransactionOutput.new(
          CML.Address.from_bech32(withdrawalScope.address),
          CML.Value.new(3_000_000n, assets),
          CML.DatumOption.new_datum(
            CML.PlutusData.from_cbor_hex(Data.to(order, SDK.EventHistoryNode)),
          ),
        ),
      );
      const replacement = CML.TransactionBody.new(
        body.inputs(),
        outputs,
        body.fee(),
      );
      const mint = CML.Mint.new();
      for (let index = 0; index < outputs.len(); index += 1)
        for (const [asset, amount] of Object.entries(
          coreToTxOutput(outputs.get(index)).assets,
        ))
          if (asset !== "lovelace")
            mint.set(
              CML.ScriptHash.from_hex(asset.slice(0, 56)),
              CML.AssetName.from_hex(asset.slice(56)),
              amount,
            );
      replacement.set_mint(mint);
      return replacement;
    });
    const changedEvents = changed.scopes.find(
      (scope) => scope.role === "forced_transaction_event",
    )!.utxos;
    const handle = await capture({
      ...changed,
      scopes: changed.scopes.map((scope) => ({
        ...scope,
        utxos:
          scope.role === "forced_transaction_event"
            ? []
            : scope.role === "withdrawal_event"
              ? changedEvents
              : scope.utxos,
      })),
      historyUnits: changed.historyUnits.map((asset) =>
        asset === oldUnit ? unit : asset,
      ),
      history: changed.history.map((entry) => ({
        ...entry,
        unit: entry.unit === oldUnit ? unit : entry.unit,
      })),
    });
    const eventKey: SDK.EventKey = {
      WithdrawalEventKey: { withdrawal_id: datum.event.id },
    };
    const fingerprint = Data.to(eventKey, SDK.EventKey);
    const source = {
      phase: "Withdrawal" as const,
      eventKey,
      fingerprint,
      entry: {
        key: datum.event.id,
        value: { ...info, validity: "IncorrectWithdrawalOwner" as const },
        keyBytes: Buffer.from(
          Data.to(datum.event.id, SDK.OutputReference),
          "hex",
        ),
        valueBytes: Buffer.from(Data.to(info, SDK.WithdrawalInfo), "hex"),
      },
    };
    const sources = new Map<
      string,
      (typeof evidence.reconstruction.sourceEvents)[number]
    >([[fingerprint, source]]);
    const current = {
      ...evidence,
      reconstruction: {
        ...evidence.reconstruction,
        sourceEventsByFingerprint: sources,
      },
    };
    const corpus: historicalCorpus.HistoricalNativeScriptCorpus = {
      schemaVersion: historicalCorpus.HISTORICAL_NATIVE_SCRIPT_CORPUS,
      throughHeaderHash: current.headerHash,
      headerHashes: [current.headerHash],
      payloadEnvelopeSha256s: [current.payloadEnvelopeSha256],
      entries: [],
      providerRosterDigest: "00".repeat(32),
      corpusDigest: "00".repeat(32),
      checkpointDigest: "00".repeat(32),
      evidenceDigest: "00".repeat(32),
    };
    // This regression isolates the real coverage/output construction. History
    // admission and detector correctness are outside its scope.
    const history = vi
      .spyOn(historicalCorpus, "requireHistoricalNativeScriptCorpus")
      .mockReturnValue({
        currentEvidence: current,
        reconstructions: [current.reconstruction],
      });
    const stop = new Error("coverage captured before detection");
    const detection = vi
      .spyOn(transitionDetection, "detectTransitionTraceFaults")
      .mockRejectedValue(stop);
    try {
      const read = async () => {
        await expect(
          replayTransitionTraceFromRetainedHistory({
            evidence: current,
            corpus,
            l1Events: handle,
          }),
        ).rejects.toBe(stop);
        const timed = detection.mock.lastCall![1];
        if (timed === undefined)
          throw new Error("detector omitted timed evidence");
        const item = timed.outOfWindowSourceEvents![0]!;
        if (item.kind !== "withdrawal")
          throw new Error("expected withdrawal coverage");
        return item;
      };
      const first = await read();
      expect(first).toEqual({
        kind: "withdrawal",
        withdrawalId: datum.event.id,
      });
      expect(first).not.toHaveProperty("validityOverride");
      first.withdrawalId.transactionId = "ff".repeat(32);
      expect((await read()).withdrawalId).toEqual(datum.event.id);
      sources.set(fingerprint, {
        ...source,
        entry: {
          ...source.entry,
          value: { ...info, validity: "IncorrectWithdrawalSignature" },
        },
      });
      expect(await read()).toEqual({
        kind: "withdrawal",
        withdrawalId: datum.event.id,
      });
      const forcedSource = evidence.reconstruction.sourceEvents.find(
        (entry) => entry.phase === "ForcedTransaction",
      );
      if (forcedSource === undefined)
        throw new Error("fixture requires forced source");
      sources.set(fingerprint, { ...forcedSource, fingerprint });
      const fallback = await read();
      expect(fallback).not.toHaveProperty("validityOverride");
      fallback.withdrawalId.outputIndex = 999n;
      expect((await read()).withdrawalId).toEqual(datum.event.id);
    } finally {
      history.mockRestore();
      detection.mockRestore();
    }
  });
});

const appendHistoryOutput = (
  snapshot: FraudProofRawL1Snapshot,
  role:
    | "deposit_event"
    | "deposit_history_data"
    | "withdrawal_event"
    | "withdrawal_history_data",
  datum: string,
  nonce: string,
  assets: Record<string, bigint>,
  reference?: FraudProofRawL1Snapshot["scopes"][number]["utxos"][number],
) => {
  const scope = snapshot.scopes.find((entry) => entry.role === role)!;
  const multi = CML.MultiAsset.new();
  const mint = CML.Mint.new();
  for (const [unit, quantity] of Object.entries(assets)) {
    if (unit === "lovelace") continue;
    multi.set(
      CML.ScriptHash.from_hex(unit.slice(0, 56)),
      CML.AssetName.from_hex(unit.slice(56)),
      quantity,
    );
    mint.set(
      CML.ScriptHash.from_hex(unit.slice(0, 56)),
      CML.AssetName.from_hex(unit.slice(56)),
      quantity,
    );
  }
  const output = CML.TransactionOutput.new(
    CML.Address.from_bech32(scope.address),
    CML.Value.new(assets.lovelace!, multi),
    CML.DatumOption.new_datum(CML.PlutusData.from_cbor_hex(datum)),
  );
  const outputs = CML.TransactionOutputList.new();
  outputs.add(output);
  const inputs = CML.TransactionInputList.new();
  inputs.add(CML.TransactionInput.new(CML.TransactionHash.from_hex(nonce), 0n));
  const body = CML.TransactionBody.new(inputs, outputs, 170_000n);
  if (Object.keys(assets).some((unit) => unit !== "lovelace"))
    body.set_mint(mint);
  if (reference !== undefined) {
    const refs = CML.TransactionInputList.new();
    const [hash, index] = reference.outRef.split("#");
    refs.add(
      CML.TransactionInput.new(
        CML.TransactionHash.from_hex(hash!),
        BigInt(index!),
      ),
    );
    body.set_reference_inputs(refs);
  }
  const txHash = CML.hash_transaction(body).to_hex();
  const raw = {
    outRef: `${txHash}#0`,
    outputCbor: output.to_canonical_cbor_hex(),
    datumCbor: coreToTxOutput(
      CML.TransactionOutput.from_cbor_hex(output.to_canonical_cbor_hex()),
    ).datum!,
    referenceScriptCbor: null,
  };
  const units = Object.keys(assets).filter((unit) => unit !== "lovelace");
  const next: FraudProofRawL1Snapshot = {
    ...snapshot,
    scopes: snapshot.scopes.map((entry) =>
      entry.role === role ? { ...entry, utxos: [...entry.utxos, raw] } : entry,
    ),
    historyUnits: [...snapshot.historyUnits, ...units],
    history: [
      ...snapshot.history,
      ...units.map((unit) => ({
        unit,
        fromGenesis: true as const,
        completeThroughPointId: snapshot.cursor.point.pointId,
        transactionHashes: [txHash],
      })),
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
            outRef: nonce + "#0",
          },
        ],
        resolvedReferenceInputs: reference === undefined ? [] : [reference],
      },
    ],
  };
  return { snapshot: next, raw };
};

const historyDepositSnapshot = async (
  external: boolean,
  kind: "deposit" | "withdrawal" = "deposit",
) => {
  const policy = (kind === "deposit" ? "62" : "63").repeat(28);
  const id = { transactionId: "a1".repeat(32), outputIndex: 0n };
  const key = await Effect.runPromise(SDK.eventHistoryKey(id));
  let payload: SDK.EventHistoryPayload = {
    DepositPayload: {
      event: {
        id,
        info: {
          l2_address: {
            paymentCredential: { PublicKeyCredential: ["aa".repeat(28)] },
            stakeCredential: null,
          },
          l2_network_id: 0n,
          l2_datum: external ? "ab".repeat(2000) : null,
        },
      },
    },
  };
  if (kind === "withdrawal") {
    const address = {
      paymentCredential: { PublicKeyCredential: ["aa".repeat(28)] as [string] },
      stakeCredential: null,
    };
    payload = {
      WithdrawalPayload: {
        event: {
          id,
          info: {
            body: {
              l2_outref: id,
              l2_owner: "aa".repeat(28),
              l2_value: new Map([["", new Map([["", 3_000_000n]])]]),
              l1_address: address,
              l1_datum: external
                ? { InlineDatum: { data: "ab".repeat(2000) } }
                : "NoDatum",
            },
            signature: ["", ""],
            validity: "WithdrawalIsValid",
          },
        },
        refund_address: address,
        refund_datum: "NoDatum",
      },
    };
  }
  let snapshot = seed;
  let reference:
    | FraudProofRawL1Snapshot["scopes"][number]["utxos"][number]
    | undefined;
  const data: SDK.EventHistoryData = {
    event_key: key,
    event_payload: Data.from(Data.to(payload, SDK.EventHistoryPayload)),
    reclaim_auth: { PublicKeyCredential: ["aa".repeat(28)] },
  };
  if (external) {
    const published = appendHistoryOutput(
      snapshot,
      kind === "deposit" ? "deposit_history_data" : "withdrawal_history_data",
      SDK.encodeEventHistoryData(data),
      "a2".repeat(32),
      { lovelace: 5_000_000n },
    );
    snapshot = published.snapshot;
    reference = published.raw;
  }
  const node: SDK.EventHistoryNode = {
    position: { Key: [key] },
    next: "ff".repeat(32),
    protected_until: 0n,
    payload: {
      Order: {
        facts: {
          event_id: id,
          inclusion_time: 1_749_999_999_000n,
          structural_lovelace: 5_000_000n,
          structural_refund_key: "aa".repeat(28),
          location: external
            ? {
                External: {
                  storage_datum_hash: SDK.eventHistoryDataHash(data),
                },
              }
            : { Inline: { payload } },
        },
      },
    },
  };
  snapshot = appendHistoryOutput(
    snapshot,
    kind === "deposit" ? "deposit_event" : "withdrawal_event",
    Data.to(node, SDK.EventHistoryNode),
    id.transactionId,
    {
      lovelace: 25_000_000n,
      [policy + key]: 1n,
      ["ab".repeat(28) + "00"]: 17n,
    },
    reference,
  ).snapshot;
  snapshot = appendHistoryOutput(
    snapshot,
    kind === "deposit" ? "deposit_event" : "withdrawal_event",
    Data.to(
      {
        position: "Root",
        next: key,
        protected_until: 0n,
        payload: "RootContent",
      },
      SDK.EventHistoryNode,
    ),
    "a3".repeat(32),
    { lovelace: 5_000_000n, [policy]: 1n },
  ).snapshot;
  snapshot = appendHistoryOutput(
    snapshot,
    kind === "deposit" ? "deposit_event" : "withdrawal_event",
    Data.to(
      {
        position: { Key: ["ff".repeat(32)] },
        next: null,
        protected_until: 0n,
        payload: { Filler: { refund_key: "aa".repeat(28) } },
      },
      SDK.EventHistoryNode,
    ),
    "a4".repeat(32),
    { lovelace: 9_000_000n, [policy + "ff".repeat(32)]: 1n },
  ).snapshot;
  return { snapshot, payload, key, policy };
};

describe("authenticated history Order capture", () => {
  it.each([
    { kind: "deposit", external: false },
    { kind: "deposit", external: true },
    { kind: "withdrawal", external: false },
    { kind: "withdrawal", external: true },
  ] as const)(
    "captures $kind original funds and filters structure; external=$external",
    async ({ kind, external }) => {
      const fixture = await historyDepositSnapshot(external, kind);
      const handle = await capture(fixture.snapshot);
      const deposits = readFreshTransitionTraceL1Events(handle).events.filter(
        (event) => event.kind === kind,
      );
      expect(deposits).toHaveLength(1);
      const event = deposits[0]!;
      if (event.kind === "forcedTransaction")
        throw new Error("unexpected forced event");
      expect(event.assetName).toBe(fixture.key);
      const opening = Data.from(
        event.history.openingCbor,
        SDK.EventHistoryOpening,
      );
      expect(opening.payload).toEqual(fixture.payload);
      expect(opening.original_assets).toEqual(
        new Map([
          ["", new Map([["", 20_000_000n]])],
          ["ab".repeat(28), new Map([["00", 17n]])],
        ]),
      );
      expect(event.retainedDataUtxo !== undefined).toBe(external);
      expect(Object.isFrozen(event.history)).toBe(true);
    },
  );
  it("rejects a hash promise without the existing retained output", async () => {
    const { snapshot } = await historyDepositSnapshot(true);
    await expect(
      capture({
        ...snapshot,
        scopes: snapshot.scopes.map((scope) =>
          scope.role === "deposit_history_data"
            ? { ...scope, utxos: [] }
            : scope,
        ),
      }),
    ).rejects.toThrow(/retained event data is unavailable/);
  });
  it("rejects a malformed full-key NFT before admitting it as an event", async () => {
    const fixture = await historyDepositSnapshot(false);
    const scope = fixture.snapshot.scopes.find(
      (entry) => entry.role === "deposit_event",
    )!;
    const order = Data.from(scope.utxos[0]!.datumCbor!, SDK.EventHistoryNode);
    const invalid = appendHistoryOutput(
      seed,
      "deposit_event",
      Data.to(order, SDK.EventHistoryNode),
      "a5".repeat(32),
      { lovelace: 25_000_000n, [fixture.policy + "00".repeat(32)]: 1n },
    ).snapshot;
    await expect(capture(invalid)).rejects.toThrow(/complete key/);
  });
});

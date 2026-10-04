import { outRefLabel } from "@al-ft/midgard-core";
import { computeHash32 } from "@al-ft/midgard-core/codec/hash";
import { wrapDaPayload } from "@al-ft/midgard-core/da-payload-envelope";
import * as SDK from "@al-ft/midgard-sdk";
import { Data } from "@lucid-evolution/lucid";
import { Effect } from "effect";
import { describe, expect, it } from "vitest";

import {
  buildTransitionFaultProof,
  reconstructDaPayload,
  submitTransitionTraceProof,
} from "../src/index.js";
import { detectSourceMembershipMismatches } from "../src/transition-trace/detect.detect-source-membership-mismatches.js";
import {
  commitCountedRoot,
  keyValuePhasProof,
  keyValuePhasRootWithCount,
} from "../src/transition-trace/phas.js";
import { buildSourceMembershipProof } from "../src/transition-trace/witnesses.build-source-membership-proof.js";
import { buildMalformedValidationRunFault } from "../src/transition-trace/witnesses.build-validation-run-faults.js";
import {
  alignedHeaderStart,
  removeAndAssertPermanentProof,
} from "./submit-init-emulator-transition-trace-subvariants.remove-and-assert-permanent-proof.js";
import {
  firstThreadUtxo,
  historyRecords,
  makeHarness,
  setupChallenge,
} from "./submit-init-emulator-transition-trace-subvariants.setup-challenge.js";
import {
  expectOnchainRefusal,
  type RefusalPin,
} from "./support/emulator/expect-onchain-refusal.js";
import { type CompleteSignedTransactionMeasurement } from "./support/emulator/measurement.js";
import {
  funderPaymentKeyHash,
  network,
} from "./support/submit-init-emulator-shared.js";
import { buildForcedAndL2Block } from "./support/transition-trace-phase-band.forced-and-l2-block.js";

type Arm = "missing" | "foreign" | "malformed" | "rawKey";
const invariant = {
  missing: "source_transaction_has_validation_run",
  foreign: "validation_run_has_source",
  malformed: "validation_run_descriptor_well_formed",
  rawKey: "validation_run_event_key_canonical",
} as const;

const landBlock = async (arm?: Arm) => {
  const context = await makeHarness();
  const { harness, publications, transitionTraceReferenceScripts } = context;
  const base = await buildForcedAndL2Block({
    forcedLast: false,
    operatorVkey: await funderPaymentKeyHash(harness.funderLucid),
    startTime: BigInt(await alignedHeaderStart(harness)),
  });
  const body = base.reconstruction.payload.block_body;
  const txId = base.reconstruction.transactions[0]!.txId;
  const eventKey: SDK.EventKey = { L2TransactionEventKey: { tx_id: txId } };
  const key = Data.to(eventKey, SDK.EventKey);
  const honestValue = body.validation_traces.find(([k]) => k === key)![1];
  const runs: SDK.DaPayloadEntry[] = body.validation_traces.flatMap(
    ([k, value]) => {
      if (k !== key || arm === undefined || arm === "rawKey")
        return [[k, value]];
      if (arm === "missing") return [];
      if (arm === "foreign")
        return [
          [
            Data.to(
              { L2TransactionEventKey: { tx_id: "ed".repeat(32) } },
              SDK.EventKey,
            ),
            value,
          ],
        ];
      return [[k, "ff"]];
    },
  );
  if (arm === "rawKey") runs.push(["00", honestValue]);
  runs.sort(([a], [b]) => (a < b ? -1 : a > b ? 1 : 0));
  const phas = await keyValuePhasRootWithCount(
    runs.map(([k, v]) => ({
      key: Buffer.from(k, "hex"),
      value: Buffer.from(v, "hex"),
    })),
  );
  const header = {
    ...base.header,
    validationTracesRoot: await commitCountedRoot({
      domain: SDK.ROOT_DOMAINS.validationTraces,
      phasRoot: phas.root,
      count: base.header.validationTraceCount,
    }),
  };
  const headerHash = await Effect.runPromise(SDK.hashBlockHeader(header));
  const payload: SDK.DaPayload = {
    ...base.reconstruction.payload,
    block_body: {
      ...body,
      header,
      header_hash: headerHash,
      validation_traces: runs,
    },
  };
  const reconstruction = await reconstructDaPayload({
    payloadEnvelopeCbor: await wrapDaPayload(SDK.encodeDaPayload(payload), {
      mode: "identity",
    }),
    committedHeader: header,
  });
  const lifecycle = await setupChallenge({
    harness,
    publications,
    transitionTraceReferenceScripts,
    header,
  });
  return { ...context, reconstruction, lifecycle, eventKey, honestValue };
};

const submit = async (
  landed: Awaited<ReturnType<typeof landBlock>>,
  fault: SDK.TransitionFault,
) =>
  await submitTransitionTraceProof({
    lucid: landed.harness.proverLucid,
    blueprint: landed.harness.realBlueprint,
    deploymentInfo: landed.lifecycle.deploymentInfo,
    network,
    signer: landed.harness.proverSigner,
    threadOutRef: outRefLabel(
      await firstThreadUtxo({
        harness: landed.harness,
        init: landed.lifecycle.init,
      }),
    ),
    proof: buildTransitionFaultProof({
      reconstruction: landed.reconstruction,
      fault,
    }),
    witnessReferenceScripts: landed.harness.witnessReferenceScripts,
    awaitConfirmation: true,
  });

const refuseHonestClaim = async (
  arm: Exclude<Arm, "rawKey">,
  pin: RefusalPin,
) => {
  const landed = await landBlock();
  expect(await detectSourceMembershipMismatches(landed.reconstruction)).toEqual(
    [],
  );
  const run = landed.reconstruction.rootData.validationTraces;
  const bytes = Buffer.from(landed.honestValue, "hex");
  let fault: SDK.TransitionFault;
  if (arm === "missing")
    fault = SDK.sourceMembershipMismatchFault({
      SourceEventMissingValidationRun: {
        source: await buildSourceMembershipProof({
          reconstruction: landed.reconstruction,
          eventKey: landed.eventKey,
        }),
        run_phas_root: run.phasRoot,
        run_absence_proof: await keyValuePhasProof(
          { ...run, root: run.phasRoot },
          Buffer.from(Data.to(landed.eventKey, SDK.EventKey), "hex"),
          bytes,
        ),
      },
    });
  else if (arm === "foreign") {
    // Supply the genuine source's inclusion proof as the false absence proof.
    const malformed = await buildMalformedValidationRunFault(
      landed.reconstruction,
      landed.eventKey,
      bytes,
    );
    if (
      !("SourceMembershipMismatch" in malformed) ||
      !("MalformedValidationRun" in malformed.SourceMembershipMismatch.witness)
    )
      throw new Error("missing run membership");
    const source = await buildSourceMembershipProof({
      reconstruction: landed.reconstruction,
      eventKey: landed.eventKey,
    });
    if (!("L2KeyOpening" in source)) throw new Error("expected L2 source");
    const opened =
      malformed.SourceMembershipMismatch.witness.MalformedValidationRun;
    fault = SDK.sourceMembershipMismatchFault({
      ForeignValidationRun: {
        event_key: Data.to(landed.eventKey, SDK.EventKey),
        value_hash: computeHash32(bytes).toString("hex"),
        run_phas_root: opened.run_phas_root,
        run_proof: opened.run_proof,
        source_phas_root: source.L2KeyOpening.phas_root,
        source_absence_proof: source.L2KeyOpening.proof,
      },
    });
  } else
    fault = await buildMalformedValidationRunFault(
      landed.reconstruction,
      landed.eventKey,
      bytes,
    );
  await expectOnchainRefusal(() => submit(landed, fault), pin);
};

const proveFault = async (arm: Arm) => {
  const landed = await landBlock(arm);
  const found = (
    await detectSourceMembershipMismatches(landed.reconstruction)
  ).find((d) => d.invariant === invariant[arm]);
  if (found === undefined || !found.buildable)
    throw new Error("N2 detection did not build the committed fault");
  const result = await submit(landed, found.fault);
  const measurements = [result.routeTxHash, result.txHash].map(
    (txHash) =>
      (
        historyRecords as {
          txHash: string;
          measurement: CompleteSignedTransactionMeasurement;
        }[]
      ).find((r) => r.txHash === txHash)!.measurement,
  );
  for (const m of measurements) {
    expect(m.completeSignedBytes).toBeLessThanOrEqual(16384);
    expect(m.executionMemory).toBeLessThanOrEqual(13200000n);
    expect(m.executionSteps).toBeLessThanOrEqual(8000000000n);
  }
  console.log(
    `N2 ${arm} route/final`,
    measurements.map((m) => ({
      bytes: m.completeSignedBytes,
      memory: m.executionMemory.toString(),
      cpu: m.executionSteps.toString(),
    })),
  );
  await removeAndAssertPermanentProof({
    harness: landed.harness,
    setup: landed.lifecycle.setup,
    deploymentInfo: landed.lifecycle.deploymentInfo,
    proofResult: result,
  });
};

describe("transition-trace validation-run faults", () => {
  it("proves missing validation run and removes the block", async () => {
    await proveFault("missing");
  }, 180000);
  it("proves foreign validation run and removes the block", async () => {
    await proveFault("foreign");
  }, 180000);
  it("proves malformed validation run and removes the block", async () => {
    await proveFault("malformed");
  }, 180000);

  it("proves an extra malformed raw validation-run key and removes the block", async () => {
    await proveFault("rawKey");
  }, 180000);
  it("refuses missing validation-run claim against the honest block", async () => {
    await refuseHonestClaim("missing", {
      refusedBy: "fraud_proofs/transition_trace/source_v1",
      check: /does_not_have/u,
    });
  }, 180000);
  it("refuses foreign validation-run claim against the honest block", async () => {
    await refuseHonestClaim("foreign", {
      refusedBy: "fraud_proofs/transition_trace/source_v1",
      check: /does_not_have/u,
    });
  }, 180000);
  it("refuses malformed validation-run claim against the honest block", async () => {
    await refuseHonestClaim("malformed", {
      refusedBy: "fraud_proofs/transition_trace/source_v1",
      check: /descriptor_bytes_are_well_formed/u,
    });
  }, 180000);
});

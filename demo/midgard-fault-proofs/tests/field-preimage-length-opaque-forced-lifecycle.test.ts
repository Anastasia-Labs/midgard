import * as SDK from "@al-ft/midgard-sdk";
import { Data } from "@lucid-evolution/lucid";
import { Effect } from "effect";
import { expect, it } from "vitest";

import { fetchFraudProofEvidence } from "../src/evidence/fraud-proof-evidence.js";
import {
  planFieldPreimageLengthCarriage,
  resolveFieldPreimageLengthCarriage,
} from "../src/field-preimage-length-mismatch/carriage.js";
import {
  submitFieldPreimageLengthForcedAuthentication,
  submitFieldPreimageLengthForcedDispatch,
  submitFieldPreimageLengthInit,
  submitFieldPreimageLengthTerminal,
} from "../src/field-preimage-length-mismatch/submit-lucid.js";
import { parseOutRef } from "../src/runtime.js";
import { inlineBodyClaim } from "./field-preimage-length-mismatch-lifecycle.forced-prepared.js";
import { expectOnChainRefusal } from "./field-preimage-length-mismatch-lifecycle.registered-contracts.js";
import {
  removeFraudulentBlock,
  setup,
} from "./field-preimage-length-mismatch-lifecycle.setup.js";
import { opaqueForcedLifecycleDaFixture } from "./field-preimage-length-opaque-forced-lifecycle.fixture.js";
import {
  authenticatedObservation,
  retainedSource,
} from "./transition-trace-challenger.build-payload-fixture.js";

it("adjudicates authenticated opaque forced bytes with the installed field-length validators", async () => {
  const preimage = Buffer.from("8180", "hex");
  const fixture = await setup({
    honestAccepted: true,
    forcedDaFixture: opaqueForcedLifecycleDaFixture,
  });
  const at = async (outRef: string) =>
    (
      await fixture.config.lucid.utxosByOutRef([
        parseOutRef(outRef, "opaque forced lifecycle output"),
      ])
    )[0];
  const raw = fixture.authenticatedForcedDa;
  if (raw === undefined)
    throw new Error("missing authenticated retained DA fixture");
  const queue = await at(fixture.fraudulent.fraudulentBlockOutRef);
  if (queue === undefined)
    throw new Error("admitted state queue block is missing");
  expect(queue.address).toBe(
    fixture.harness.contracts.stateQueue.spendingScriptAddress,
  );
  expect(queue.assets[fixture.fraudulent.stateQueueBlockUnit]).toBe(1n);
  const queueState = await Effect.runPromise(
    SDK.utxoToStateQueueUTxO(
      queue,
      fixture.harness.contracts.stateQueue.policyId,
    ),
  );
  const admittedHeader = await Effect.runPromise(
    SDK.getHeaderFromStateQueueDatum(queueState.datum),
  );
  expect(admittedHeader).toEqual(raw.header);
  expect(admittedHeader.prevHeaderHash).toBe(SDK.GENESIS_HEADER_HASH);
  expect(await Effect.runPromise(SDK.hashBlockHeader(admittedHeader))).toBe(
    raw.headerHash,
  );
  expect(fixture.fraudulent.headerHash).toBe(raw.headerHash);

  const routed = await fetchFraudProofEvidence({
    observation: { ...authenticatedObservation(raw), header: admittedHeader },
    sources: [retainedSource(raw)],
  });
  if (routed.kind !== "field_preimage_length_mismatch")
    throw new Error("expected authenticated raw field-length evidence");
  const evidence = routed.evidence;
  expect(evidence.prepared).toMatchObject({
    headerHash: raw.headerHash,
    sourceKind: "forced",
    direction: "wrongfulAcceptance",
    fieldIndex: 0,
    preimageHex: "8180",
    actualLength: 2,
    declaredLength: 3,
  });
  const header = evidence.stageEvidence.forcedHeader;
  const membership = evidence.stageEvidence.forcedMembership;
  if (header === undefined || membership === undefined)
    throw new Error("missing authenticated forced anchors");
  expect(header).toEqual(admittedHeader);
  expect(membership.root).toBe(admittedHeader.forcedTransactionsRoot);
  expect(membership.count).toBe(1n);
  expect(membership.value.tx_id).toBe(evidence.prepared.transactionId);
  expect(membership.value.verdict).toBe("ForcedTxValid");
  const workflow = { config: fixture.config };
  const plan = planFieldPreimageLengthCarriage({ workflow, evidence });
  expect(plan.plan.tier).toBe("Inline");
  expect(plan.plan.inlinePreimage).toEqual(preimage);
  const carriage = await resolveFieldPreimageLengthCarriage({
    workflow,
    evidence,
  });
  expect(carriage.carriageReferences).toEqual([]);
  const claim = carriage.claimResolver(carriage.carriageReferences);
  expect(claim).toEqual(inlineBodyClaim(0, preimage));

  const init = await submitFieldPreimageLengthInit({
    config: fixture.config,
    fraudulentBlockOutRef: fixture.fraudulent.fraudulentBlockOutRef,
  });
  const dispatch = await submitFieldPreimageLengthForcedDispatch({
    config: fixture.config,
    threadOutRef: init.nextThreadOutRef,
    direction: 0n,
  });
  await expectOnChainRefusal(
    () =>
      submitFieldPreimageLengthForcedAuthentication({
        config: fixture.config,
        threadOutRef: dispatch.nextThreadOutRef,
        header,
        membership,
        prepared: evidence.prepared,
        claim: inlineBodyClaim(0, Buffer.from("8080", "hex")),
      }),
    "opaque forced field commitment substitution",
  );
  const authentication = await submitFieldPreimageLengthForcedAuthentication({
    config: fixture.config,
    threadOutRef: dispatch.nextThreadOutRef,
    header,
    membership,
    prepared: evidence.prepared,
    claim,
    claimResolver: carriage.claimResolver,
    carriageReferenceInputs: carriage.carriageReferences,
  });
  const thread = await at(authentication.nextThreadOutRef);
  if (thread === undefined)
    throw new Error("authenticated computation thread is missing");
  const terminal = await submitFieldPreimageLengthTerminal({
    config: fixture.config,
    threadOutRef: authentication.nextThreadOutRef,
  });
  const proof = await at(terminal.fraudProofOutRef);
  if (proof === undefined)
    throw new Error("installed terminal did not mint the fraud proof");
  expect(proof.address).toBe(
    fixture.config.contracts.fraudProof.spendingScriptAddress,
  );
  expect(proof.assets[terminal.fraudProofUnit]).toBe(1n);
  expect(proof.assets.lovelace).toBe(thread.assets.lovelace);
  expect(proof.datum).toBe(
    Data.to(
      { fraud_prover: fixture.config.signer.paymentKeyHash },
      SDK.FraudProofTokenDatum,
    ),
  );
  expect(await at(authentication.nextThreadOutRef)).toBeUndefined();
  expect(await at(fixture.fraudulent.fraudulentBlockOutRef)).toEqual(queue);
  const removal = await removeFraudulentBlock(fixture);
  expect(removal.result.transactions.map(({ kind }) => kind)).toEqual([
    "remove-target",
  ]);
  expect(removal.result.awaitedConfirmation).toBe(true);
  expect(removal.result.stateQueueBlockOutRef).toBe(
    fixture.fraudulent.fraudulentBlockOutRef,
  );
  expect(removal.result.fraudulentHeaderHash).toBe(raw.headerHash);
  expect(removal.result.fraudProofOutRef).toBe(terminal.fraudProofOutRef);
  expect(removal.result.layout.fraudProofRefInputIndex).not.toBeNull();
  expect(
    Number(removal.result.layout.fraudProofRefInputIndex),
  ).toBeGreaterThanOrEqual(0);
  expect(await at(fixture.fraudulent.fraudulentBlockOutRef)).toBeUndefined();
  // The protocol's fraud-proof spend validator always refuses consumption.
  // Removal reads this permanent witness; only the computation thread is burnt.
  expect(await at(terminal.fraudProofOutRef)).toEqual(proof);
}, 120_000);

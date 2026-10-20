/** The prover funding permit refuses a validation-trace dispute VerifySource
 * other than the one its builder evaluated, before it is submitted: a bond
 * above the deployment-bound bond, a second bond, the bond at another
 * contract, at a wallet outside the deployment or under another datum, a
 * case output above its derived lovelace, a contract input besides the
 * pending claim, an authority naming another header, deployment, case
 * lovelace or bond datum than the claim's, or the evaluated bytes without
 * the builder's authority. The workflow stalls with the refusal after Init
 * and Open. A pending claim naming a fraud prover other than the funding
 * wallet's key is refused when the permit derives the bond from it.
 *
 * The category is not installed until its defender exists; this file
 * stands in for that install so the production classifier mints the
 * objective's actuation permit. */
import * as SDK from "@al-ft/midgard-sdk";
import { type CML, Data, type UTxO } from "@lucid-evolution/lucid";
import { beforeEach, describe, expect, it, vi } from "vitest";

import { VALIDATION_TRACE_CASE_OPEN } from "../src/validation-trace-dispute/adapter.js";
import {
  assertValidationDisputeOpener,
  readValidationDisputeFundingAuthority,
} from "../src/workflow/funding-reservation-permit.assert-validation-dispute-funding.js";
import { exactResolvedOutputCbor } from "../src/workflow/funding-reservation-permit.create-workflow-funding-reservation-permit.js";
import { parseOwnOutRef } from "../src/workflow/own-outputs.js";
import { workflowPreflightTransaction } from "../src/workflow/transaction-boundary.js";
import { drainProver } from "./support/installed-prover-wallet.js";
import {
  openFundedDispute,
  turnUntil,
} from "./support/validation-trace-dispute-funded-runner.js";
import {
  ADA,
  confirmedStages,
  FLOOR,
  FUNDED,
  isVerifySource,
  TIMEOUT,
} from "./support/validation-trace-dispute-funded-verify-source.js";
import {
  BOND_LOVELACE,
  type Scene,
  setUpScene,
} from "./support/validation-trace-dispute-open-scene.js";
import {
  bondIndex,
  caseIndex,
  changeIndex,
  inlineDatumOf,
  presentVerifySource,
  type VerifySourcePresentation,
  type VerifySourceTamper,
  withAddress,
  withCoin,
  withDatum,
} from "./support/validation-trace-dispute-tampered-verify-source.js";

// Stands in for the family's install, which waits on its defender.
vi.mock("../src/workflow/installed-categories.js", async (importOriginal) => {
  const actual =
    await importOriginal<
      typeof import("../src/workflow/installed-categories.js")
    >();
  return {
    ...actual,
    isInstalledFraudProofCategory: (
      category: Parameters<typeof actual.isInstalledFraudProofCategory>[0],
    ) =>
      category === "validationTraceDispute" ||
      actual.isInstalledFraudProofCategory(category),
  };
});

let scene: Scene;
/** A wallet outside the deployment: the funder's. */
let funderAddress: string;

const BINDING =
  "validation dispute funding authority differs from its exact VerifySource action or deployment terms";
const ANOTHER = "00".repeat(32);

/** The case output one lovelace over the case's, paid from the change. */
const caseOneOver: VerifySourceTamper = (outputs, addresses) => {
  const at = caseIndex(outputs, addresses.case);
  const change = changeIndex(outputs, addresses.wallet);
  return outputs.map((output, index) =>
    index === at
      ? withCoin(output, output.amount().coin() + 1n)
      : index === change
        ? withCoin(output, output.amount().coin() - 1n)
        : output,
  );
};

/** A bond datum for another case. */
const anotherBondDatum = (datumCborHex: string): string => {
  const bond = Data.from(datumCborHex, SDK.DisputeBond);
  return Data.to(
    {
      ...bond,
      case_id:
        (bond.case_id.startsWith("00") ? "01" : "00") + bond.case_id.slice(2),
    },
    SDK.DisputeBond,
  );
};

/** The bond output under a datum for another case. */
const bondForAnotherCase: VerifySourceTamper = (outputs, addresses) => {
  const bond = bondIndex(outputs, addresses.bond);
  return outputs.map((output, index) =>
    index === bond
      ? withDatum(output, anotherBondDatum(inlineDatumOf(output)))
      : output,
  );
};

/** A live output at the source contract that no workflow of ours made. */
const foreignSourceOutput = async (): Promise<UTxO> => {
  const { h } = scene;
  const lucid = h.funderLucid;
  const address = scene.chain.source.spendingScriptAddress;
  const signed = await (
    await lucid
      .newTx()
      .pay.ToContract(
        address,
        { kind: "inline", value: Data.void() },
        { lovelace: 5n * ADA },
      )
      .complete()
  ).sign
    .withWallet()
    .complete();
  const hash = await signed.submit();
  await lucid.awaitTx(hash);
  const [output] = await lucid.utxosByOutRef([
    { txHash: hash, outputIndex: 0 },
  ]);
  if (output?.address !== address)
    throw new Error("the source output is not at output 0");
  return output;
};

describe("validation-trace dispute VerifySource refused by the prover funding permit", () => {
  // Each case disputes the scene's failing block from a fresh deployment,
  // its prover wallet split into a collateral output and plain funding.
  beforeEach(async () => {
    scene = await setUpScene();
    funderAddress = await scene.h.funderLucid.wallet().address();
  }, TIMEOUT);

  it.each([
    {
      name: "a bond one lovelace over the deployment-bound bond",
      present: (): VerifySourcePresentation => ({
        outputs: (
          outputs: CML.TransactionOutput[],
          addresses: { bond: string; wallet: string },
        ) => {
          const bond = bondIndex(outputs, addresses.bond);
          const change = changeIndex(outputs, addresses.wallet);
          return outputs.map((output, index) =>
            index === bond
              ? withCoin(output, output.amount().coin() + 1n)
              : index === change
                ? withCoin(output, output.amount().coin() - 1n)
                : output,
          );
        },
      }),
      refusal: `validation dispute bond output differs from the deployment-bound bond: actual=${(BOND_LOVELACE + 1n).toString()} bound=${BOND_LOVELACE.toString()}`,
    },
    {
      name: "a second bond output",
      present: (): VerifySourcePresentation => ({
        outputs: (
          outputs: CML.TransactionOutput[],
          addresses: { bond: string; wallet: string },
        ) => {
          const bond = outputs[bondIndex(outputs, addresses.bond)]!;
          const change = changeIndex(outputs, addresses.wallet);
          return [
            ...outputs.map((output, index) =>
              index === change
                ? withCoin(output, output.amount().coin() - BOND_LOVELACE)
                : output,
            ),
            bond,
          ];
        },
      }),
      refusal:
        "validation dispute VerifySource must pay exactly one bond and one case output: bonds=2 cases=1",
    },
    {
      name: "the bond paid to another dispute contract",
      present: (): VerifySourcePresentation => ({
        outputs: (
          outputs: CML.TransactionOutput[],
          addresses: { bond: string; wallet: string },
        ) => {
          const bond = bondIndex(outputs, addresses.bond);
          return outputs.map((output, index) =>
            index === bond
              ? withAddress(output, scene.chain.source.spendingScriptAddress)
              : output,
          );
        },
      }),
      refusal:
        "validation dispute VerifySource pays custody beyond its bond and case",
    },
    {
      name: "the bond paid to a wallet outside the deployment",
      present: (): VerifySourcePresentation => ({
        outputs: (
          outputs: CML.TransactionOutput[],
          addresses: { bond: string; wallet: string },
        ) => {
          const bond = bondIndex(outputs, addresses.bond);
          return outputs.map((output, index) =>
            index === bond ? withAddress(output, funderAddress) : output,
          );
        },
      }),
      refusal: "funding output escapes the governed contract roster",
    },
    {
      name: "the bond under a datum for another case",
      present: (): VerifySourcePresentation => ({
        outputs: bondForAnotherCase,
      }),
      refusal:
        "validation dispute bond output differs from its exact marker and datum",
    },
    {
      name: "the case output one lovelace over the case's lovelace",
      present: (): VerifySourcePresentation => ({ outputs: caseOneOver }),
      refusal:
        "validation dispute case output differs from its exact floor: actual=",
    },
    {
      name: "a contract input besides its pending claim",
      present: async (): Promise<VerifySourcePresentation> => ({
        inputs: [await foreignSourceOutput()],
      }),
      refusal:
        "validation dispute VerifySource consumes a contract input besides its pending claim",
    },
    {
      name: "an authority naming another header",
      present: (): VerifySourcePresentation => ({
        authority: (grant) => ({ ...grant, headerHash: ANOTHER }),
      }),
      refusal: `${BINDING} (header hash)`,
    },
    {
      name: "an authority bound to another deployment",
      present: (): VerifySourcePresentation => ({
        authority: (grant) => ({ ...grant, deploymentFingerprint: ANOTHER }),
      }),
      refusal: `${BINDING} (deployment fingerprint)`,
    },
    {
      name: "an authority and a case output one lovelace over the case's lovelace",
      present: (): VerifySourcePresentation => ({
        outputs: caseOneOver,
        authority: (grant) => ({
          ...grant,
          caseOutput: {
            ...grant.caseOutput,
            lovelace: grant.caseOutput.lovelace + 1n,
          },
        }),
      }),
      refusal: `${BINDING} (case lovelace)`,
    },
    {
      name: "an authority and a bond under a datum for another case",
      present: (): VerifySourcePresentation => ({
        outputs: bondForAnotherCase,
        authority: (grant) => ({
          ...grant,
          bond: {
            ...grant.bond,
            datumCborHex: anotherBondDatum(grant.bond.datumCborHex),
          },
        }),
      }),
      refusal: `${BINDING} (bond datum)`,
    },
    {
      name: "the evaluated bytes presented without the builder's authority",
      present: () => null,
      refusal: "funding transaction exceeds its governed custody allocation",
    },
  ])(
    "refuses VerifySource with $name",
    async ({ present, refusal }) => {
      await drainProver(scene.h as never, FUNDED);
      const presentation = await present();
      const dispute = await openFundedDispute(scene);
      try {
        const { result, history } = await turnUntil(
          dispute,
          VALIDATION_TRACE_CASE_OPEN,
          { wrap: presentVerifySource(scene, presentation) },
        );
        expect(result.kind, history.join("\n")).toBe("stalled");
        if (result.kind !== "stalled") return;
        expect(result.reason).toContain("funding reservation failed for");
        expect(result.reason).toContain(refusal);
        expect(confirmedStages(result)).toStrictEqual(["init", "open"]);
      } finally {
        await dispute.close();
      }
    },
    TIMEOUT,
  );

  // The claim on the ledger is the funding wallet's own, so the foreign
  // prover is put into the claim the permit derives the bond from.
  it(
    "refuses a pending claim naming a fraud prover other than the funding wallet's key, where the permit derives the bond from it",
    async () => {
      await drainProver(scene.h as never, FUNDED);
      const dispute = await openFundedDispute(scene);
      const foreign = "ab".repeat(28);
      let refusal: unknown = "not reached";
      try {
        await turnUntil(dispute, VALIDATION_TRACE_CASE_OPEN, {
          wrap: (adapter) => ({
            ...adapter,
            preflight: async (context) => {
              const preflight = await adapter.preflight(context);
              if (!isVerifySource(context.action)) return preflight;
              const authority = readValidationDisputeFundingAuthority(
                workflowPreflightTransaction(preflight)!,
              )!;
              const [opener] = await scene.h.emulator.getUtxosByOutRef([
                parseOwnOutRef(authority.openerOutRef),
              ]);
              const pending = Data.from(
                opener!.datum!,
                SDK.PendingValidationClaimDatum,
              );
              const claimed: UTxO = {
                ...opener!,
                datum: Data.to(
                  { ...pending, fraud_prover: foreign },
                  SDK.PendingValidationClaimDatum,
                ),
              };
              const bond = Data.from(
                authority.bondDatumCborHex,
                SDK.DisputeBond,
              );
              try {
                assertValidationDisputeOpener({
                  authority: {
                    ...authority,
                    openerResolvedOutputCborHex:
                      exactResolvedOutputCbor(claimed),
                    bondDatumCborHex: Data.to(
                      { ...bond, fraud_prover: foreign },
                      SDK.DisputeBond,
                    ),
                  },
                  terms: {
                    bondLovelace: BOND_LOVELACE.toString(),
                    caseFloorLovelace: FLOOR.toString(),
                    bondAddress: scene.chain.disputeBond.spendingScriptAddress,
                    caseAddress: scene.chain.case.spendingScriptAddress,
                    casePolicyId: scene.chain.case.policyId,
                  },
                  opener: claimed,
                  fundingPaymentKeyHash: scene.h.proverSigner.paymentKeyHash,
                });
                refusal = null;
              } catch (error) {
                refusal = error;
              }
              return preflight;
            },
          }),
        });
      } finally {
        await dispute.close();
      }
      expect(refusal).toBeInstanceOf(Error);
      expect((refusal as Error).message).toBe(`${BINDING} (fraud prover)`);
    },
    TIMEOUT,
  );
});

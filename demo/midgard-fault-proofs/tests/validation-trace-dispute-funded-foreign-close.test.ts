/** The close of our case after another party verified it and nobody
 * defended it, under the prover funding permit, on the emulator. Our funded
 * watcher opens our claim and signs our VerifySource, and its process stops
 * before broadcasting it; another party runs VerifySource over our pending
 * claim first, and nobody defends. Until the defense deadline has passed
 * the next process holds the case open, and a close valid from the last
 * slot not past the deadline is refused by the case validator. Once it has
 * passed, our runner closes the case Convicted, spending the case and bond
 * that other party's VerifySource created, read from the case token's L1
 * history: the record names us and the bond and the case's lovelace come
 * back to us. The close's funding authority binds it to the case our Open
 * opened: a close for a case we did not open is refused at the authority.
 *
 * The category is not installed until its defender exists; this file
 * stands in for that install so the production classifier mints the
 * objective's actuation permit. */
import { computeDeploymentManifestJsonDigest } from "@al-ft/midgard-core/deployment-manifest-identity";
import * as SDK from "@al-ft/midgard-sdk";
import { credentialToAddress, Data, type UTxO } from "@lucid-evolution/lucid";
import { afterAll, beforeAll, describe, expect, it } from "vitest";

import {
  VALIDATION_TRACE_CASE_CONVICTED,
  VALIDATION_TRACE_CASE_OPEN,
} from "../src/validation-trace-dispute/adapter.js";
import { validationTraceCaseOrigin } from "../src/validation-trace-dispute/case-origin.js";
import {
  buildValidationTraceClose,
  validationTraceFraudProofUnit,
} from "../src/validation-trace-dispute/close.js";
import { validationTraceDefenseDeadlineMs } from "../src/validation-trace-dispute/waiting.js";
import { grantValidationDisputeFundingAuthority } from "../src/workflow/funding-reservation-permit.assert-validation-dispute-funding.js";
import { grantValidationDisputeMoveAuthority } from "../src/workflow/funding-reservation-permit.assert-validation-dispute-move.js";
import { exactResolvedOutputCbor } from "../src/workflow/funding-reservation-permit.create-workflow-funding-reservation-permit.js";
import { VALIDATION_DISPUTE_CLOSE_BINDING } from "../src/workflow/funding-reservation-permit.validation-dispute-close-authority.js";
import type { FraudProofWorkflowAction } from "../src/workflow/orchestrator.js";
import { parseOwnOutRef } from "../src/workflow/own-outputs.js";
import { expectOnchainRefusal } from "./support/emulator/expect-onchain-refusal.js";
import { network } from "./support/submit-init-emulator-shared.js";
import {
  outRefOf,
  spentBy,
} from "./support/validation-trace-dispute-foreign-verify.js";
import {
  closeAuthorityChecks,
  type ForeignClose,
  openForeignVerifiedCase,
  otherToken,
} from "./support/validation-trace-dispute-funded-close.js";
import { refusalOf } from "./support/validation-trace-dispute-funded-move-walk.js";
import {
  confirmedMoves,
  recordedOutput,
} from "./support/validation-trace-dispute-funded-moves.js";
import { turnUntil } from "./support/validation-trace-dispute-funded-runner.js";
import { TIMEOUT } from "./support/validation-trace-dispute-funded-verify-source.js";
import { BOND_LOVELACE } from "./support/validation-trace-dispute-open-scene.js";

let journey: ForeignClose;

const liveAt = async (outRef: string): Promise<UTxO | undefined> =>
  (
    await journey.scene.h.emulator.getUtxosByOutRef([parseOwnOutRef(outRef)])
  )[0];

const deadlineOf = (caseOutput: UTxO) =>
  validationTraceDefenseDeadlineMs(Data.from(caseOutput.datum!, SDK.Case));

describe("validation-trace dispute close of our case after another party verified it", () => {
  beforeAll(async () => {
    journey = await openForeignVerifiedCase();
  }, TIMEOUT);

  afterAll(async () => {
    await journey?.dispute.close();
    journey?.recorder.restore();
  });

  it(
    "holds the case open until its defense deadline has passed, and a close valid from the last slot not past it is refused at case_can_close",
    async () => {
      const { scene, dispute, verified, artifact, closes, capture } = journey;
      const { h } = scene;
      const lucid = h.proverLucid;
      const deadline = deadlineOf(verified.caseOutput);
      // The last slot whose time is not past the deadline.
      h.emulator.awaitSlot(
        lucid.unixTimeToSlot(deadline) - lucid.currentSlot(),
      );
      const validFrom = lucid.slotToUnixTime(lucid.currentSlot());
      expect(validFrom).toBeLessThanOrEqual(deadline);
      expect(deadline - validFrom).toBeLessThan(1_000);
      const { tx } = await buildValidationTraceClose({
        lucid,
        signer: h.proverSigner,
        deployment: dispute.deployment,
        artifact,
        caseOutRef: outRefOf(verified.caseOutput),
        bondOutRef: outRefOf(verified.bond),
        validFrom,
      });
      await expectOnchainRefusal(() => tx.complete({ localUPLCEval: true }), {
        refusedBy: "fraud_proofs/validation_trace/case_v1",
        check: /case_can_close/u,
      });
      const held = await dispute.turn(capture);
      expect(held).toMatchObject({ kind: "pending" });
      expect("reason" in held && held.reason).toMatch(
        new RegExp(`^${VALIDATION_TRACE_CASE_OPEN}`, "u"),
      );
      expect(closes).toHaveLength(0);
      expect(await liveAt(outRefOf(verified.caseOutput))).toBeDefined();
    },
    TIMEOUT,
  );

  it(
    "closes our case Convicted once its defense deadline has passed, spending the case and bond the other party's VerifySource created: the record names us, and the bond and the case's lovelace come back to us",
    async () => {
      const { scene, dispute, verified, removal, opener, artifact, closes } =
        journey;
      const { h } = scene;
      const lucid = h.proverLucid;
      const caseOutRef = outRefOf(verified.caseOutput);
      const bondOutRef = outRefOf(verified.bond);
      const ahead =
        lucid.unixTimeToSlot(deadlineOf(verified.caseOutput)) -
        lucid.currentSlot() +
        1;
      if (ahead > 0) h.emulator.awaitSlot(ahead);
      // The L1 view stops here, before our record.
      removal.holdView();
      await removal
        .l1For(scene.failing.evidence.headerHash)
        .observe({ headerHash: scene.failing.evidence.headerHash });
      const { result, history } = await turnUntil(
        dispute,
        VALIDATION_TRACE_CASE_CONVICTED,
        { limit: 12, wrap: journey.capture },
      );
      expect(result.kind, history.join("\n")).toBe("pending");
      const stages = confirmedMoves(result);
      expect(stages.map(({ stage }) => stage)).toStrictEqual([
        "init",
        "open",
        "close",
      ]);
      expect(closes).toHaveLength(1);
      const [close] = closes;
      expect(close!.action.input.consumes).toBe(caseOutRef);
      expect(close!.authority).toMatchObject({
        caseOpenerOutRef: opener,
        caseOutRef,
        bondOutRef,
      });
      expect(spentBy(close!.signed.toTransaction().to_cbor_hex())).toEqual(
        expect.arrayContaining([caseOutRef, bondOutRef]),
      );
      expect(await liveAt(caseOutRef)).toBeUndefined();
      expect(await liveAt(bondOutRef)).toBeUndefined();

      const proofUnit = validationTraceFraudProofUnit(
        dispute.deployment,
        artifact.headerHash,
      );
      const record = (await liveAt(stages[2]!.outputs[0]!))!;
      expect(record.address).toBe(h.contracts.fraudProof.spendingScriptAddress);
      expect(record.assets).toEqual({ lovelace: 2_000_000n, [proofUnit]: 1n });
      expect(
        Data.from(record.datum!, SDK.FraudProofTokenDatum).fraud_prover,
      ).toBe(h.proverSigner.paymentKeyHash);
      const ours = credentialToAddress(network, {
        type: "Key",
        hash: h.proverSigner.paymentKeyHash,
      });
      const closeHash = close!.authority.transactionHash;
      const [refund, remainder] = await h.emulator.getUtxosByOutRef([
        { txHash: closeHash, outputIndex: 0 },
        { txHash: closeHash, outputIndex: 1 },
      ]);
      expect(refund!.address).toBe(ours);
      expect(refund!.assets).toEqual({ lovelace: BOND_LOVELACE });
      expect(remainder!.address).toBe(ours);
    },
    TIMEOUT,
  );

  it(
    "reads our case's case and bond from the other party's VerifySource in the case token's L1 history, and refuses a history holding no VerifySource",
    async () => {
      const { scene, dispute, verified, artifact } = journey;
      const caseUnit = Object.keys(verified.caseOutput.assets).find((unit) =>
        unit.startsWith(scene.chain.case.policyId),
      )!;
      const transactions = await dispute.caseHistory.transactions({
        headerHash: artifact.headerHash,
        caseUnit,
      });
      expect(
        validationTraceCaseOrigin({ transactions, caseUnit }),
      ).toStrictEqual({
        caseOutRef: outRefOf(verified.caseOutput),
        bondOutRef: outRefOf(verified.bond),
      });
      const verifySourceHash = verified.caseOutput.txHash;
      expect(
        await refusalOf(() =>
          validationTraceCaseOrigin({
            transactions: transactions.filter(
              ({ txHash }) => txHash !== verifySourceHash,
            ),
            caseUnit,
          }),
        ),
      ).toBe(
        `validation-trace case ${caseUnit} has no single VerifySource in its L1 history`,
      );
    },
    TIMEOUT,
  );

  describe("the close's funding authority", () => {
    const ANOTHER = "00".repeat(32);
    const checks = () => closeAuthorityChecks(journey);
    const refused = (...names: string[]) =>
      `${VALIDATION_DISPUTE_CLOSE_BINDING} (${names.join(", ")})`;
    const closed = () => checks().closed();
    const bind = (input?: Parameters<ReturnType<typeof checks>["bind"]>[0]) =>
      checks().bind(input);
    const walk = (input?: Parameters<ReturnType<typeof checks>["walk"]>[0]) =>
      checks().walk(input);
    const presentInputs = (
      ...input: Parameters<ReturnType<typeof checks>["presentInputs"]>
    ) => checks().presentInputs(...input);
    const readCase = (lineage: unknown) => checks().readCase(lineage);
    const openLineage = () => checks().openLineage();
    const spentInputs = () => checks().spentInputs();
    const regranted = () => checks().regranted();

    it("admits the close the watcher built as its builder evaluated it, its case and bond as released custody", async () => {
      const { scene, verified } = journey;
      expect(await refusalOf(() => bind())).toBeNull();
      expect(readCase(openLineage())).toBe(
        Object.keys(verified.caseOutput.assets)
          .find((unit) => unit.startsWith(scene.chain.case.policyId))!
          .slice(56),
      );
      expect(await refusalOf(() => presentInputs((spent) => spent))).toBeNull();
      expect(
        (await walk()).map((lineage) => (lineage as { outRef: string }).outRef),
      ).toStrictEqual([outRefOf(verified.caseOutput), outRefOf(verified.bond)]);
    });

    it.each<{ name: string; check: () => unknown; refusal: string }>([
      {
        name: "another workflow's category",
        check: () => bind({ overrides: { category: "doubleSpend" } }),
        refusal: refused("category"),
      },
      {
        name: "another category's policy",
        check: () => bind({ policyCategory: "doubleSpend" }),
        refusal: refused("policy category"),
      },
      {
        name: "a policy without dispute terms",
        check: () => bind({ withoutTerms: true }),
        refusal: refused("dispute terms"),
      },
      {
        name: "a reservation begun for another action kind",
        check: () =>
          bind({ overrides: { currentActionKind: "verify_source" } }),
        refusal: refused("current action kind"),
      },
      {
        name: "an action of another kind than a close",
        check: () => {
          const action = {
            ...closed().action,
            input: { ...closed().action.input, stage: "verify_source" },
          } as FraudProofWorkflowAction;
          return bind({
            action,
            overrides: {
              currentActionDigest: computeDeploymentManifestJsonDigest(action),
            },
          });
        },
        refusal: refused("action kind"),
      },
      {
        name: "an action other than the one the reservation began",
        check: () =>
          bind({
            action: {
              ...closed().action,
              actionId: `${closed().action.actionId}'`,
            },
          }),
        refusal: refused("action digest"),
      },
      {
        name: "an authority over another case than its action spends",
        check: () =>
          bind({
            authority: { ...closed().authority, caseOutRef: `${ANOTHER}#0` },
          }),
        refusal: refused("case"),
      },
      {
        name: "an authority for another header than the actuation's",
        check: () => bind({ headerHash: ANOTHER }),
        refusal: refused("header hash"),
      },
      {
        name: "an authority for another deployment",
        check: () =>
          bind({
            authority: {
              ...closed().authority,
              deploymentFingerprint: ANOTHER,
            },
          }),
        refusal: refused("deployment fingerprint"),
      },
      {
        name: "a close for a case no Open of ours opened",
        check: () => walk({ lineage: null }),
        refusal: refused("case opener lineage"),
      },
      {
        name: "a close for a case another key's claim opened",
        check: () => {
          const claim = recordedOutput(journey.scene, journey.opener);
          const pending = Data.from(
            claim.datum!,
            SDK.PendingValidationClaimDatum,
          );
          return readCase({
            ...openLineage(),
            resolvedOutputCborHex: exactResolvedOutputCbor({
              ...claim,
              datum: Data.to(
                { ...pending, fraud_prover: ANOTHER.slice(0, 56) },
                SDK.PendingValidationClaimDatum,
              ),
            }),
          });
        },
        refusal: refused("case opener prover"),
      },
      {
        name: "a spent case other than the authority's exact output",
        check: () =>
          presentInputs((spent) => spent, {
            caseResolvedOutputCborHex: exactResolvedOutputCbor(
              journey.verified.bond,
            ),
          }),
        refusal: refused("case input"),
      },
      {
        name: "a spent case holding another case's token: a case our Open did not open",
        check: () =>
          presentInputs(({ caseInput, bond }) => ({
            caseInput: otherToken(caseInput, journey.scene.chain.case.policyId),
            bond,
          })),
        refusal: refused("case input token"),
      },
      {
        name: "a spent bond other than the authority's exact output",
        check: () =>
          presentInputs((spent) => spent, {
            bondResolvedOutputCborHex: exactResolvedOutputCbor(
              journey.verified.caseOutput,
            ),
          }),
        refusal: refused("bond input"),
      },
      {
        name: "a spent bond holding another case's marker",
        check: () =>
          presentInputs(({ caseInput, bond }) => ({
            caseInput,
            bond: otherToken(bond, journey.scene.chain.case.policyId),
          })),
        refusal: refused("bond input marker"),
      },
      {
        name: "a close that omits its case",
        check: () =>
          walk({
            inputs: new Map([
              [outRefOf(journey.verified.bond), journey.verified.bond],
            ]),
          }),
        refusal: "validation dispute case close omits its case or its bond",
      },
      {
        name: "a close that omits its bond",
        check: () =>
          walk({
            inputs: new Map([
              [
                outRefOf(journey.verified.caseOutput),
                journey.verified.caseOutput,
              ],
            ]),
          }),
        refusal: "validation dispute case close omits its case or its bond",
      },
      {
        name: "a close that consumes another contract input",
        check: () =>
          walk({
            inputs: new Map([
              ...spentInputs(),
              [`${ANOTHER}#0`, journey.verified.caseOutput],
            ]),
          }),
        refusal:
          "validation dispute case close consumes a contract input besides its case and bond",
      },
      {
        name: "a close that tops up protocol state",
        check: () => walk({ topup: 1n }),
        refusal:
          "validation dispute case close tops up protocol state: topup=1",
      },
      {
        name: "a close that also carries a fraud slash",
        check: () => walk({ slash: true }),
        refusal:
          "funding transaction carries both a fraud slash and a validation dispute close authority",
      },
      {
        name: "a close that also carries a thread move authority",
        check: async () => {
          const signed = await regranted();
          grantValidationDisputeMoveAuthority({
            signed,
            kind: "timeout",
            deploymentFingerprint: closed().authority.deploymentFingerprint,
            headerHash: closed().authority.headerHash,
            caseOpenerOutRef: journey.opener,
            defenderVkh: "00".repeat(28),
            thread: journey.verified.caseOutput,
          });
          return await walk({ signed });
        },
        refusal:
          "funding transaction carries a case close authority beside another validation dispute authority",
      },
      {
        name: "a close that also carries a VerifySource authority",
        check: async () => {
          const signed = await regranted();
          const { address } = journey.verified.caseOutput;
          grantValidationDisputeFundingAuthority({
            signed,
            deploymentFingerprint: closed().authority.deploymentFingerprint,
            headerHash: closed().authority.headerHash,
            opener: journey.verified.caseOutput,
            bond: { address, unit: "", lovelace: 0n, datumCborHex: "" },
            caseOutput: { address, unit: "", floorLovelace: 0n, lovelace: 0n },
          });
          return await walk({ signed });
        },
        refusal:
          "funding transaction carries a case close authority beside another validation dispute authority",
      },
    ])("refuses $name", async ({ check, refusal }) => {
      expect(await refusalOf(check)).toBe(refusal);
    });
  });
});

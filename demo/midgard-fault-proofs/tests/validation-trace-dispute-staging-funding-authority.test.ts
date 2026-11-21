/** The staging authority the prover funding permit admits our staging of
 * a staged builtin step's operands, and its spend back, by: bound to its
 * action, header and deployment, carried alone, never beside another
 * funding authority (a fraud slash, a VerifySource, a thread move or a case
 * close), and over a transaction that mints nothing; under
 * reconciliation-only authority, a spend back alone. */
import { createHash } from "node:crypto";

import { computeDeploymentManifestJsonDigest } from "@al-ft/midgard-core/deployment-manifest-identity";
import {
  Emulator,
  generateEmulatorAccount,
  Lucid,
  mintingPolicyToId,
  paymentCredentialOf,
  scriptFromNative,
  type TxSigned,
} from "@lucid-evolution/lucid";
import { describe, expect, it } from "vitest";

import { grantValidationDisputeFundingAuthority } from "../src/workflow/funding-reservation-permit.assert-validation-dispute-funding.js";
import { grantValidationDisputeCloseAuthority } from "../src/workflow/funding-reservation-permit.validation-dispute-close-authority.js";
import { grantValidationDisputeMoveAuthority } from "../src/workflow/funding-reservation-permit.validation-dispute-move-authority.js";
import {
  assertValidationDisputeStagingBinding,
  grantValidationDisputeStagingAuthority,
  VALIDATION_DISPUTE_RECONCILIATION_RECLAIM_ONLY,
  VALIDATION_DISPUTE_STAGING_BINDING,
  type ValidationDisputeStagingAuthority,
  validationDisputeStagingFunding,
  type ValidationDisputeStagingShape,
} from "../src/workflow/funding-reservation-permit.validation-dispute-staging-authority.js";
import {
  ADA,
  NEXT,
  policy,
  reclaimShape,
  RUN,
  STAGED_OUT_REF,
  stagedOutput,
  stagingShape,
  theirs,
  THREAD_OUT_REF,
} from "./support/validation-trace-dispute-staging-funding.js";

const BESIDE =
  "funding transaction carries a validation dispute staging authority beside another funding authority";
const MINTS = "validation dispute staging mints";
const identity = { deploymentFingerprint: "fingerprint", headerHash: "header" };
const payer = generateEmulatorAccount({ lovelace: 100n * ADA });

describe("the staging authority's binding", () => {
  const run = (datum: string) =>
    createHash("sha256").update(Buffer.from(datum, "hex")).digest("hex");
  const stageActionOf = (datum: string) =>
    ({
      actionId: "validation-trace-stage:run:0",
      input: { stage: "stage_operands", run: run(datum) },
    }) as never;
  const stageAction = stageActionOf(RUN);
  const reclaimAction = {
    actionId: `validation-trace-reclaim-staging:${STAGED_OUT_REF}`,
    input: { stage: "reclaim_staging", consumes: STAGED_OUT_REF },
  } as never;
  const authorityOf = (
    shape: ValidationDisputeStagingShape,
  ): ValidationDisputeStagingAuthority =>
    ({
      category: "validationTraceDispute",
      deploymentFingerprint: "fingerprint",
      headerHash: "header",
      shape,
    }) as never;
  const bind = (
    shape: ValidationDisputeStagingShape,
    action: never,
    {
      kind = shape.kind as string,
      headerHash = "header",
      fingerprint = "fingerprint",
    } = {},
  ) =>
    assertValidationDisputeStagingBinding({
      authority: authorityOf(shape),
      policy: {
        ...(policy as object),
        deploymentFingerprint: fingerprint,
      } as never,
      state: {
        category: "validationTraceDispute",
        currentActionKind: kind,
        currentActionDigest: computeDeploymentManifestJsonDigest(action),
      },
      action,
      headerHash,
    });

  it("binds a staging to its action's run, and a spend back to the output its action spends, under this header and deployment", () => {
    bind(stagingShape(), stageAction);
    bind(reclaimShape, reclaimAction);
  });

  it("refuses a staging authority for another run, a datum that is no staging run, another action, header or deployment", () => {
    for (const [refused, names] of [
      [() => bind(stagingShape(NEXT), stageAction), "run"],
      [() => bind(stagingShape("00"), stageActionOf("00")), "run"],
      [() => bind(stagingShape(), reclaimAction), "action kind, run"],
      [
        () => bind(stagingShape(), stageAction, { headerHash: "other" }),
        "header hash",
      ],
      [
        () => bind(stagingShape(), stageAction, { fingerprint: "other" }),
        "deployment fingerprint",
      ],
      [
        () =>
          bind(
            { ...reclaimShape, outRef: `${"ab".repeat(32)}#1` } as never,
            reclaimAction,
          ),
        "staging output",
      ],
    ] as const)
      expect(refused).toThrow(
        `${VALIDATION_DISPUTE_STAGING_BINDING} (${names})`,
      );
  });
});

/** A signed transaction of ours granted a staging authority of `shape`,
 * a staging by default; minting a token under our key's own policy when
 * `mints`. */
const staged = async ({
  shape = { datumCborHex: RUN },
  mints = false,
}: {
  readonly shape?: Parameters<
    typeof grantValidationDisputeStagingAuthority
  >[0]["shape"];
  readonly mints?: boolean;
} = {}): Promise<TxSigned> => {
  const lucid = await Lucid(new Emulator([payer]), "Custom");
  lucid.selectWallet.fromSeed(payer.seedPhrase);
  const policy = scriptFromNative({
    type: "sig",
    keyHash: paymentCredentialOf(payer.address).hash,
  });
  const pay = lucid
    .newTx()
    .pay.ToAddress(theirs.address, { lovelace: 2n * ADA });
  const signed = await (
    await (
      mints
        ? pay
            .mintAssets({ [`${mintingPolicyToId(policy)}01`]: 1n })
            .attach.MintingPolicy(policy)
        : pay
    ).complete()
  ).sign
    .withWallet()
    .complete();
  grantValidationDisputeStagingAuthority({ signed, ...identity, shape });
  return signed;
};

describe("the prover funding permit over our staging beside another funding authority", () => {
  it.each([
    { name: "a fraud slash", slash: true, beside: () => {} },
    {
      name: "a VerifySource authority",
      slash: false,
      beside: (signed: TxSigned) =>
        grantValidationDisputeFundingAuthority({
          signed,
          ...identity,
          opener: stagedOutput,
          bond: {
            address: stagedOutput.address,
            unit: "",
            lovelace: 0n,
            datumCborHex: "",
          },
          caseOutput: {
            address: stagedOutput.address,
            unit: "",
            floorLovelace: 0n,
            lovelace: 0n,
          },
        }),
    },
    {
      name: "a thread move authority",
      slash: false,
      beside: (signed: TxSigned) =>
        grantValidationDisputeMoveAuthority({
          signed,
          kind: "timeout",
          ...identity,
          caseOpenerOutRef: THREAD_OUT_REF,
          defenderVkh: "00".repeat(28),
          thread: stagedOutput,
        }),
    },
    {
      name: "a case close authority",
      slash: false,
      beside: (signed: TxSigned) =>
        grantValidationDisputeCloseAuthority({
          signed,
          ...identity,
          caseOpenerOutRef: THREAD_OUT_REF,
          caseInput: stagedOutput,
          bond: stagedOutput,
        }),
    },
  ])("refuses a staging that also carries $name", async ({ slash, beside }) => {
    const signed = await staged();
    beside(signed);
    await expect(
      validationDisputeStagingFunding({
        signed,
        slash,
        reconciling: false,
        policy: {} as never,
        state: {} as never,
        action: {} as never,
      }),
    ).rejects.toThrow(BESIDE);
  });

  it.each([
    { name: "a staging", shape: { datumCborHex: RUN } },
    { name: "a spend back", shape: { output: stagedOutput } },
  ])("refuses $name that mints", async ({ shape }) => {
    for (const mints of [true, false]) {
      const checked = validationDisputeStagingFunding({
        signed: await staged({ shape, mints }),
        slash: false,
        reconciling: false,
        policy: {} as never,
        state: {} as never,
        action: {} as never,
      });
      if (mints) await expect(checked).rejects.toThrow(MINTS);
      else await expect(checked).rejects.not.toThrow(MINTS);
    }
  });

  it("refuses, under reconciliation-only authority, a transaction that is no spend back, and passes a spend back on to its binding", async () => {
    const reconciling = async (signed: TxSigned) =>
      await validationDisputeStagingFunding({
        signed,
        slash: false,
        reconciling: true,
        policy: {} as never,
        state: {} as never,
        action: {} as never,
      });
    await expect(reconciling(await staged())).rejects.toThrow(
      VALIDATION_DISPUTE_RECONCILIATION_RECLAIM_ONLY,
    );
    await expect(
      reconciling(await staged({ shape: { output: stagedOutput } })),
    ).rejects.not.toThrow(VALIDATION_DISPUTE_RECONCILIATION_RECLAIM_ONLY);
    // Bytes with no staging authority at all.
    const lucid = await Lucid(new Emulator([payer]), "Custom");
    lucid.selectWallet.fromSeed(payer.seedPhrase);
    const bare = await (
      await lucid
        .newTx()
        .pay.ToAddress(theirs.address, { lovelace: 2n * ADA })
        .complete()
    ).sign
      .withWallet()
      .complete();
    await expect(reconciling(bare)).rejects.toThrow(
      VALIDATION_DISPUTE_RECONCILIATION_RECLAIM_ONLY,
    );
  });

  it("passes a staging under its own authority alone on to its binding", async () => {
    await expect(
      validationDisputeStagingFunding({
        signed: await staged(),
        slash: false,
        reconciling: false,
        policy: {} as never,
        state: {} as never,
        action: {} as never,
      }),
    ).rejects.not.toThrow(BESIDE);
  });
});

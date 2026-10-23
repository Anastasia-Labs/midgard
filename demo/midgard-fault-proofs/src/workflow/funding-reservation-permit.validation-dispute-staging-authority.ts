/**
 * The validation-trace dispute's staging funding authority.
 *
 * Before a thread's PrepareSelected move at a staged builtin step, each run
 * of the step's staged operands is staged as one output at our staging
 * address, locked by the native script `sig(<funding key hash>)`, and once
 * no thread reads it the output is spent back by that key
 * (validation-trace-dispute/one-step-staging.ts). The staging address is
 * outside the governed contract roster, and the spend back carries that
 * native script as a witness, so the runtime funding bound refuses both
 * without this authority. It admits exactly two shapes:
 *
 * - a staging, which spends only our wallet's reserved plain coins and pays
 *   exactly one output at our staging address, holding the run's exact
 *   inline datum and lovelace only, no more than the run's deposit (its
 *   minimum ada), the rest back to our wallet. The reserved coins it spends
 *   are this staging's alone; a staging that does not build for want of
 *   our wallet's ada raises the operator's alarm instead
 *   (validation-trace-dispute/one-step-staging-funding.ts), and nothing
 *   here holds back a staging that builds;
 * - a spend back, which spends exactly one staging output of ours, the one
 *   this reservation's lineage shows a staging of ours left, with that one
 *   native script as its only script witness, and pays our wallet only.
 *
 * It binds the exact signed bytes evaluated locally and is minted only by
 * the staging builder, for category 00000006.
 */
import { createHash } from "node:crypto";

import { computeDeploymentManifestJsonDigest } from "@al-ft/midgard-core/deployment-manifest-identity";
import {
  midgardCekStagingDeposit,
  midgardCekStagingRunChunks,
  midgardCekStagingScript,
} from "@al-ft/midgard-sdk";
import {
  CML,
  type TxSigned,
  type UTxO,
  validatorToScriptHash,
} from "@lucid-evolution/lucid";

import { assertWorkflowActuationPermitIdentity } from "./actuation-permit.js";
import { readValidationDisputeFundingAuthority } from "./funding-reservation-permit.assert-validation-dispute-funding.js";
import {
  actionKind,
  exactResolvedOutputCbor,
  parseConfirmedActionOutput,
} from "./funding-reservation-permit.create-workflow-funding-reservation-permit.js";
import { readValidationDisputeCloseAuthority } from "./funding-reservation-permit.validation-dispute-close-authority.js";
import {
  failing,
  type Policy,
  readValidationDisputeMoveAuthority,
} from "./funding-reservation-permit.validation-dispute-move-authority.js";
import type { PermitState } from "./funding-reservation-permit.workflow-funding-reservation-port.js";
import type { FraudProofWorkflowAction } from "./orchestrator.js";

const CATEGORY = "validationTraceDispute" as const;
const STAGE = "stage_operands" as const;
const RECLAIM = "reclaim_staging" as const;

/** A staging: the run it stages. */
export type ValidationDisputeStagingRun = Readonly<{
  kind: typeof STAGE;
  /** The inline datum of the run this transaction stages. */
  datumCborHex: string;
}>;

/** A spend back: the one staging output of ours it spends. */
export type ValidationDisputeStagingReclaim = Readonly<{
  kind: typeof RECLAIM;
  outRef: string;
  resolvedOutputCborHex: string;
}>;

export type ValidationDisputeStagingShape =
  | ValidationDisputeStagingRun
  | ValidationDisputeStagingReclaim;

export type ValidationDisputeStagingAuthority = Readonly<{
  category: typeof CATEGORY;
  deploymentFingerprint: string;
  headerHash: string;
  shape: ValidationDisputeStagingShape;
  transactionHash: string;
  transactionBodySha256: string;
  signedTransactionCborHex: string;
}>;

/** In this process only, keyed by the builder's own TxSigned, as VerifySource. */
const authorities = new WeakMap<TxSigned, ValidationDisputeStagingAuthority>();

const sha256 = (hex: string): string =>
  createHash("sha256").update(Buffer.from(hex, "hex")).digest("hex");

const bodySha256 = (signed: TxSigned): string =>
  sha256(signed.toTransaction().body().to_cbor_hex());

const outRefLabel = (utxo: Pick<UTxO, "txHash" | "outputIndex">) =>
  `${utxo.txHash}#${utxo.outputIndex.toString()}`;

/** Called by the staging builder once it has signed a staging or a spend
 * back; a spend back names the staging output it spends. */
export const grantValidationDisputeStagingAuthority = ({
  signed,
  deploymentFingerprint,
  headerHash,
  shape,
}: {
  readonly signed: TxSigned;
  readonly deploymentFingerprint: string;
  readonly headerHash: string;
  readonly shape:
    | Omit<ValidationDisputeStagingRun, "kind">
    | Readonly<{ output: UTxO }>;
}): void => {
  const transaction = signed.toTransaction();
  authorities.set(
    signed,
    Object.freeze({
      category: CATEGORY,
      deploymentFingerprint,
      headerHash,
      shape: Object.freeze(
        "output" in shape
          ? {
              kind: RECLAIM,
              outRef: outRefLabel(shape.output),
              resolvedOutputCborHex: exactResolvedOutputCbor(shape.output),
            }
          : { kind: STAGE, datumCborHex: shape.datumCborHex },
      ),
      transactionHash: CML.hash_transaction(transaction.body()).to_hex(),
      transactionBodySha256: bodySha256(signed),
      signedTransactionCborHex: transaction.to_cbor_hex(),
    }),
  );
};

export const readValidationDisputeStagingAuthority = (
  signed: TxSigned,
): ValidationDisputeStagingAuthority | null => {
  const authority = authorities.get(signed);
  if (authority === undefined) return null;
  const transaction = signed.toTransaction();
  if (
    signed.toHash().toLowerCase() !== authority.transactionHash ||
    CML.hash_transaction(transaction.body()).to_hex() !==
      authority.transactionHash ||
    transaction.to_cbor_hex() !== authority.signedTransactionCborHex ||
    bodySha256(signed) !== authority.transactionBodySha256
  )
    throw new Error(
      "signed validation dispute staging changed after local evaluation",
    );
  return authority;
};

export const VALIDATION_DISPUTE_STAGING_BINDING =
  "validation dispute staging authority differs from its exact staging or deployment terms";

/**
 * The authority's binding to this workflow and its action. Each refusal
 * names the failed binding.
 */
export const assertValidationDisputeStagingBinding = ({
  authority,
  policy,
  state,
  action,
  headerHash,
}: {
  readonly authority: ValidationDisputeStagingAuthority;
  readonly policy: Policy;
  readonly state: Pick<
    PermitState,
    "category" | "currentActionKind" | "currentActionDigest"
  >;
  readonly action: FraudProofWorkflowAction;
  /** The actuation permit's challenged header. */
  readonly headerHash: string;
}): void => {
  const { shape } = authority;
  const differing = failing([
    ["category", state.category === CATEGORY],
    ["policy category", policy.category === CATEGORY],
    ["current action kind", state.currentActionKind === shape.kind],
    ["action kind", actionKind(action) === shape.kind],
    [
      "action digest",
      state.currentActionDigest === computeDeploymentManifestJsonDigest(action),
    ],
    ["header hash", authority.headerHash === headerHash],
    [
      "deployment fingerprint",
      authority.deploymentFingerprint === policy.deploymentFingerprint,
    ],
    shape.kind === STAGE
      ? [
          "run",
          action.input.run === sha256(shape.datumCborHex) &&
            midgardCekStagingRunChunks(shape.datumCborHex) !== null,
        ]
      : ["staging output", action.input.consumes === shape.outRef],
  ]);
  if (differing.length > 0)
    throw new Error(
      `${VALIDATION_DISPUTE_STAGING_BINDING} (${differing.join(", ")})`,
    );
};

/**
 * The staging `shape`'s funding, checked step by step as the runtime bound
 * walks its signed transaction: `lineage` is what this reservation's
 * lineage shows for a spend back's staging output.
 */
export const validationDisputeStagingWalk = ({
  shape,
  policy,
  state,
  lineage,
}: {
  readonly shape: ValidationDisputeStagingShape;
  readonly policy: Pick<Policy, "fundingPaymentKeyHash" | "protocolParameters">;
  readonly state: Pick<PermitState, "snapshot">;
  readonly lineage: unknown;
}) => {
  const walletAddress = state.snapshot.walletAddress;
  const scriptHash = validatorToScriptHash(
    midgardCekStagingScript(policy.fundingPaymentKeyHash),
  );
  // Our staging address, on our wallet's network.
  const address = CML.EnterpriseAddress.new(
    CML.Address.from_bech32(walletAddress).network_id(),
    CML.Credential.new_script(CML.ScriptHash.from_hex(scriptHash)),
  )
    .to_address()
    .to_bech32();
  const coinsPerUtxoByte = BigInt(policy.protocolParameters.coinsPerUtxoByte);
  const deposit = (datumCbor: string): bigint =>
    midgardCekStagingDeposit({ address, datumCbor, coinsPerUtxoByte });
  if (shape.kind === RECLAIM) {
    const confirmed =
      lineage === null ? undefined : parseConfirmedActionOutput(lineage);
    if (
      confirmed?.sourceActionKind !== STAGE ||
      confirmed.outRef !== shape.outRef ||
      confirmed.resolvedOutputCborHex !== shape.resolvedOutputCborHex
    )
      throw new Error(
        "validation dispute staging spend back lacks the lineage of a staging of ours",
      );
  }
  let staged = 0;
  let spent = 0;
  return Object.freeze({
    /** A spend back's one native script, ours; nothing else, and nothing
     * for a staging. */
    admitsScriptWitnesses: (witnesses: CML.TransactionWitnessSet): boolean => {
      const natives = witnesses.native_scripts();
      return (
        shape.kind === RECLAIM &&
        natives?.len() === 1 &&
        natives.get(0).hash().to_hex() === scriptHash &&
        (witnesses.plutus_v1_scripts()?.len() ?? 0) +
          (witnesses.plutus_v2_scripts()?.len() ?? 0) +
          (witnesses.plutus_v3_scripts()?.len() ?? 0) ===
          0
      );
    },
    /** Any input besides our wallet's coins: a spend back's own staging
     * output, and nothing else. */
    admitInput: (outRef: string, input: UTxO): void => {
      if (shape.kind !== RECLAIM)
        throw new Error(
          "validation dispute staging spends an input besides our wallet's coins",
        );
      if (
        spent !== 0 ||
        outRef !== shape.outRef ||
        exactResolvedOutputCbor(input) !== shape.resolvedOutputCborHex ||
        input.address !== address ||
        typeof input.datum !== "string" ||
        midgardCekStagingRunChunks(input.datum) === null ||
        input.scriptRef != null ||
        Object.keys(input.assets).some((unit) => unit !== "lovelace")
      )
        throw new Error(
          "validation dispute staging spend back spends an input besides its staging output",
        );
      spent += 1;
    },
    /** A staging's one output at our staging address as custody; our
     * wallet's change as the bound counts it; nothing else. */
    output: (
      raw: CML.TransactionOutput,
    ): Readonly<{ custody: bigint; allocation: bigint }> | null => {
      const to = raw.address().to_bech32();
      if (to === walletAddress) return null;
      if (shape.kind !== STAGE || to !== address)
        throw new Error(
          "validation dispute staging pays an output besides its staging output and our wallet",
        );
      const coin = raw.amount().coin();
      if (
        staged !== 0 ||
        raw.datum()?.as_datum()?.to_cbor_hex() !== shape.datumCborHex ||
        raw.script_ref() !== undefined ||
        raw.amount().has_multiassets() ||
        coin > deposit(shape.datumCborHex)
      )
        throw new Error(
          "validation dispute staging output differs from its exact run",
        );
      staged += 1;
      return { custody: coin, allocation: coin };
    },
    assertComplete: (): void => {
      if ((shape.kind === STAGE ? staged : spent) !== 1)
        throw new Error("validation dispute staging omits its staging output");
    },
  });
};

/**
 * Our staging's or its spend back's funding under the authority its signed
 * transaction carries, checked step by step as the runtime bound walks it;
 * `null` when it carries none. Neither mints.
 */
export const validationDisputeStagingFunding = async ({
  signed,
  slash,
  policy,
  state,
  action,
}: {
  readonly signed: TxSigned;
  /** Whether the transaction also carries a fraud slash authority. */
  readonly slash: boolean;
  readonly policy: Policy;
  readonly state: PermitState;
  readonly action: FraudProofWorkflowAction;
}) => {
  const authority = readValidationDisputeStagingAuthority(signed);
  if (authority === null) return null;
  if (
    slash ||
    readValidationDisputeFundingAuthority(signed) !== null ||
    readValidationDisputeMoveAuthority(signed) !== null ||
    readValidationDisputeCloseAuthority(signed) !== null
  )
    throw new Error(
      "funding transaction carries a validation dispute staging authority beside another funding authority",
    );
  // Neither a staging nor its spend back mints: our key's native script
  // admitted on a spend back is no minting policy.
  if (signed.toTransaction().body().mint() !== undefined)
    throw new Error(
      "validation dispute staging mints under the staging authority",
    );
  assertValidationDisputeStagingBinding({
    authority,
    policy,
    state,
    action,
    headerHash: assertWorkflowActuationPermitIdentity({
      permit: state.actuationPermit,
      category: state.category,
      rollbackGeneration: state.snapshot.rollbackGeneration,
    }).headerHash,
  });
  const { shape } = authority;
  return validationDisputeStagingWalk({
    shape,
    policy,
    state,
    lineage:
      shape.kind === RECLAIM
        ? await state.port.resolveConfirmedInput({ outRef: shape.outRef })
        : null,
  });
};

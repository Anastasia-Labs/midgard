/**
 * The validation-trace dispute's VerifySource funding authority.
 *
 * VerifySource locks the deployment's dispute bond and raises the pending
 * claim to the case floor; neither is released custody or a min-Ada top-up,
 * so the runtime funding bound refuses the transaction without this
 * authority. The authority admits exactly one bond output, at the signed
 * deployment's bond, and one case output, at the larger of the claim's
 * lovelace and the case floor, which the compiled profile's constant fixes
 * rather than the signed deployment; both are derived again here from the
 * spent pending claim and the policy's dispute terms. It binds the exact
 * signed bytes evaluated locally and is minted only by the VerifySource
 * builder, for category 00000006.
 */
import { createHash } from "node:crypto";

import { computeDeploymentManifestJsonDigest } from "@al-ft/midgard-core/deployment-manifest-identity";
import {
  DisputeBond,
  PendingValidationClaimDatum,
  validationCaseBondMarkerName,
  validationCaseId,
} from "@al-ft/midgard-sdk";
import {
  CML,
  coreToTxOutput,
  Data,
  type TxOutput,
  type TxSigned,
  type UTxO,
} from "@lucid-evolution/lucid";

import { assertWorkflowActuationPermitIdentity } from "./actuation-permit.js";
import {
  actionKind,
  exactResolvedOutputCbor,
} from "./funding-reservation-permit.create-workflow-funding-reservation-permit.js";
import type { PermitState } from "./funding-reservation-permit.workflow-funding-reservation-port.js";
import type { FraudProofWorkflowAction } from "./orchestrator.js";
import type { readWorkflowRuntimeFundingPolicy } from "./runtime-funding-policy.js";

export const VALIDATION_DISPUTE_FUNDING_CATEGORY =
  "validationTraceDispute" as const;
const VERIFY_SOURCE = "verify_source";

export type ValidationDisputeFundingAuthority = Readonly<{
  category: typeof VALIDATION_DISPUTE_FUNDING_CATEGORY;
  deploymentFingerprint: string;
  headerHash: string;
  openerOutRef: string;
  openerResolvedOutputCborHex: string;
  bondAddress: string;
  bondUnit: string;
  bondLovelace: string;
  bondDatumCborHex: string;
  caseAddress: string;
  caseUnit: string;
  caseFloorLovelace: string;
  caseLovelace: string;
  transactionHash: string;
  transactionBodySha256: string;
  signedTransactionCborHex: string;
}>;

type Policy = ReturnType<typeof readWorkflowRuntimeFundingPolicy>;

/**
 * Each authority lives only in this process, keyed by the builder's own
 * TxSigned object: it is never journaled or persisted, and a re-read
 * transaction carries none. The runtime bound checks it while the funding
 * permit prepares the transaction, before the signed bytes are journaled
 * with their submission intent; recovery then resubmits those saved bytes
 * and needs no authority again.
 */
const authorities = new WeakMap<TxSigned, ValidationDisputeFundingAuthority>();

const bodySha256 = (signed: TxSigned): string =>
  createHash("sha256")
    .update(Buffer.from(signed.toTransaction().body().to_cbor_hex(), "hex"))
    .digest("hex");

const outRefLabel = (utxo: Pick<UTxO, "txHash" | "outputIndex">) =>
  `${utxo.txHash}#${utxo.outputIndex.toString()}`;

/**
 * Called by the VerifySource builder once it has evaluated and signed the
 * transaction; the values are the ones the builder paid.
 */
export const grantValidationDisputeFundingAuthority = ({
  signed,
  deploymentFingerprint,
  headerHash,
  opener,
  bond,
  caseOutput,
}: {
  readonly signed: TxSigned;
  readonly deploymentFingerprint: string;
  readonly headerHash: string;
  readonly opener: UTxO;
  readonly bond: Readonly<{
    address: string;
    unit: string;
    lovelace: bigint;
    datumCborHex: string;
  }>;
  readonly caseOutput: Readonly<{
    address: string;
    unit: string;
    floorLovelace: bigint;
    lovelace: bigint;
  }>;
}): void => {
  const transaction = signed.toTransaction();
  authorities.set(
    signed,
    Object.freeze({
      category: VALIDATION_DISPUTE_FUNDING_CATEGORY,
      deploymentFingerprint,
      headerHash,
      openerOutRef: outRefLabel(opener),
      openerResolvedOutputCborHex: exactResolvedOutputCbor(opener),
      bondAddress: bond.address,
      bondUnit: bond.unit,
      bondLovelace: bond.lovelace.toString(),
      bondDatumCborHex: bond.datumCborHex,
      caseAddress: caseOutput.address,
      caseUnit: caseOutput.unit,
      caseFloorLovelace: caseOutput.floorLovelace.toString(),
      caseLovelace: caseOutput.lovelace.toString(),
      transactionHash: CML.hash_transaction(transaction.body()).to_hex(),
      transactionBodySha256: bodySha256(signed),
      signedTransactionCborHex: transaction.to_cbor_hex(),
    }),
  );
};

export const readValidationDisputeFundingAuthority = (
  signed: TxSigned,
): ValidationDisputeFundingAuthority | null => {
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
      "signed validation dispute VerifySource changed after local evaluation",
    );
  return authority;
};

const refuse = (names: readonly string[]): never => {
  throw new Error(
    `validation dispute funding authority differs from its exact VerifySource action or deployment terms (${names.join(", ")})`,
  );
};

/**
 * The authority's binding to this workflow, its action and the policy's
 * dispute terms: the deployment-bound bond and addresses, and the compiled
 * profile's case floor. Each refusal names the failed binding.
 */
export const assertValidationDisputeFundingAuthorityBinding = ({
  authority,
  policy,
  state,
  action,
  headerHash,
}: {
  readonly authority: ValidationDisputeFundingAuthority;
  readonly policy: Policy;
  readonly state: PermitState;
  readonly action: FraudProofWorkflowAction;
  /** The actuation permit's challenged header. */
  readonly headerHash: string;
}): NonNullable<Policy["validationDispute"]> => {
  const terms = policy.validationDispute;
  const differing = (
    [
      ["category", state.category === VALIDATION_DISPUTE_FUNDING_CATEGORY],
      [
        "policy category",
        policy.category === VALIDATION_DISPUTE_FUNDING_CATEGORY,
      ],
      ["dispute terms", terms !== undefined],
      ["current action kind", state.currentActionKind === VERIFY_SOURCE],
      ["action kind", actionKind(action) === VERIFY_SOURCE],
      [
        "action digest",
        state.currentActionDigest ===
          computeDeploymentManifestJsonDigest(action),
      ],
      ["opener", action.input.consumes === authority.openerOutRef],
      ["header hash", authority.headerHash === headerHash],
      [
        "deployment fingerprint",
        authority.deploymentFingerprint === policy.deploymentFingerprint,
      ],
      ["bond", authority.bondLovelace === terms?.bondLovelace],
      ["bond address", authority.bondAddress === terms?.bondAddress],
      ["case address", authority.caseAddress === terms?.caseAddress],
      ["case floor", authority.caseFloorLovelace === terms?.caseFloorLovelace],
    ] as const
  )
    .filter(([, holds]) => !holds)
    .map(([name]) => name);
  if (differing.length > 0 || terms === undefined) refuse(differing);
  return terms!;
};

/**
 * Derives the bond and case outputs again from the spent pending claim: the
 * claim must be the authority's, name the actuation's header and the funding
 * wallet as its fraud prover, and fix the case id the bond datum and both
 * tokens carry. The case holds max(claim lovelace, case floor).
 */
export const assertValidationDisputeOpener = ({
  authority,
  terms,
  opener,
  fundingPaymentKeyHash,
}: {
  readonly authority: ValidationDisputeFundingAuthority;
  readonly terms: NonNullable<Policy["validationDispute"]>;
  readonly opener: UTxO;
  readonly fundingPaymentKeyHash: string;
}): bigint => {
  if (exactResolvedOutputCbor(opener) !== authority.openerResolvedOutputCborHex)
    refuse(["opener output"]);
  if (opener.datum == null) refuse(["opener datum"]);
  const pending = Data.from(opener.datum!, PendingValidationClaimDatum);
  if (pending.data === null) refuse(["opener datum"]);
  const state = pending.data!;
  const caseId = validationCaseId(
    state.challenged_header_hash,
    state.claim.event_to_step_membership.key,
    opener,
  );
  const openerLovelace = opener.assets.lovelace ?? 0n;
  const floor = BigInt(terms.caseFloorLovelace);
  const caseLovelace = openerLovelace > floor ? openerLovelace : floor;
  const differing = (
    [
      [
        "challenged header",
        state.challenged_header_hash === authority.headerHash,
      ],
      ["fraud prover", pending.fraud_prover === fundingPaymentKeyHash],
      ["case unit", authority.caseUnit === terms.casePolicyId + caseId],
      [
        "bond unit",
        authority.bondUnit ===
          terms.casePolicyId + validationCaseBondMarkerName(caseId),
      ],
      [
        "bond datum",
        authority.bondDatumCborHex ===
          Data.to(
            {
              challenged_header_hash: state.challenged_header_hash,
              fraud_prover: pending.fraud_prover,
              case_id: caseId,
            },
            DisputeBond,
          ),
      ],
      ["case lovelace", authority.caseLovelace === caseLovelace.toString()],
    ] as const
  )
    .filter(([, holds]) => !holds)
    .map(([name]) => name);
  if (differing.length > 0) refuse(differing);
  return openerLovelace;
};

const exactAssets = (
  output: TxOutput,
  expected: Readonly<Record<string, bigint>>,
): boolean => {
  const units = Object.keys(output.assets);
  return (
    units.length === Object.keys(expected).length &&
    units.every((unit) => output.assets[unit] === expected[unit])
  );
};

/**
 * Classifies a body output under the authority. An output at the bond or
 * case address must be exactly the authority's; any other output there is
 * refused with what differs.
 */
export const validationDisputeOutput = ({
  authority,
  raw,
  opener,
}: {
  readonly authority: ValidationDisputeFundingAuthority;
  readonly raw: CML.TransactionOutput;
  readonly opener: UTxO;
}): "bond" | "case" | null => {
  const output = coreToTxOutput(raw);
  if (output.address === authority.bondAddress) {
    const coin = raw.amount().coin();
    if (coin !== BigInt(authority.bondLovelace))
      throw new Error(
        `validation dispute bond output differs from the deployment-bound bond: actual=${coin.toString()} bound=${authority.bondLovelace}`,
      );
    if (
      !exactAssets(output, {
        lovelace: coin,
        [authority.bondUnit]: 1n,
      }) ||
      output.datum !== authority.bondDatumCborHex ||
      output.datumHash != null ||
      output.scriptRef != null
    )
      throw new Error(
        "validation dispute bond output differs from its exact marker and datum",
      );
    return "bond";
  }
  if (output.address === authority.caseAddress) {
    const coin = raw.amount().coin();
    if (coin !== BigInt(authority.caseLovelace))
      throw new Error(
        `validation dispute case output differs from its exact floor: actual=${coin.toString()} bound=${authority.caseLovelace}`,
      );
    const carried = Object.fromEntries(
      Object.entries(opener.assets).filter(([unit]) => unit !== "lovelace"),
    );
    if (
      !exactAssets(output, {
        ...carried,
        lovelace: coin,
        [authority.caseUnit]: 1n,
      }) ||
      output.datum == null ||
      output.scriptRef != null
    )
      throw new Error(
        "validation dispute case output differs from its exact case and thread tokens",
      );
    return "case";
  }
  return null;
};

/**
 * VerifySource's funding under its authority, checked step by step as the
 * runtime bound walks the signed transaction; `null` when it carries none,
 * so the bound refuses its custody as before.
 */
export const validationDisputeFunding = ({
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
  const authority = readValidationDisputeFundingAuthority(signed);
  if (authority === null) return null;
  if (slash)
    throw new Error(
      "funding transaction carries both a fraud slash and a validation dispute authority",
    );
  const terms = assertValidationDisputeFundingAuthorityBinding({
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
  let opener: UTxO | undefined;
  let openerLovelace = 0n;
  let bonds = 0;
  let cases = 0;
  return Object.freeze({
    /** Finds the spent pending claim and derives its outputs again. */
    admitOpener: (inputs: ReadonlyMap<string, UTxO>): void => {
      opener = inputs.get(authority.openerOutRef);
      if (opener === undefined)
        throw new Error(
          "validation dispute VerifySource omits its pending claim",
        );
      openerLovelace = assertValidationDisputeOpener({
        authority,
        terms,
        opener,
        fundingPaymentKeyHash: policy.fundingPaymentKeyHash,
      });
    },
    /** The only contract input VerifySource spends is its pending claim. */
    admitContractInput: (outRef: string): void => {
      if (outRef !== authority.openerOutRef)
        throw new Error(
          "validation dispute VerifySource consumes a contract input besides its pending claim",
        );
    },
    /**
     * A bond or case output's custody and allocation: the bond is new
     * custody at its bound value; the case keeps the claim's released
     * lovelace and is raised to the compiled profile's case floor. `null`
     * for any other output.
     */
    output: (
      raw: CML.TransactionOutput,
    ): Readonly<{ custody: bigint; allocation: bigint }> | null => {
      const kind = validationDisputeOutput({ authority, raw, opener: opener! });
      if (kind === null) return null;
      const coin = raw.amount().coin();
      if (kind === "bond") bonds += 1;
      else cases += 1;
      return {
        custody: coin,
        allocation: kind === "bond" ? coin : coin - openerLovelace,
      };
    },
    /** Custody at any other proof-thread contract. */
    refuseOtherCustody: (): never => {
      throw new Error(
        "validation dispute VerifySource pays custody beyond its bond and case",
      );
    },
    /** Like a fraud slash, VerifySource tops up no protocol state. */
    refuseProtocolTopup: (protocolTopup: bigint): void => {
      if (protocolTopup !== 0n)
        throw new Error(
          `validation dispute VerifySource tops up protocol state: topup=${protocolTopup.toString()}`,
        );
    },
    /**
     * Exactly one bond and one case. The claim is the only contract input
     * admitted, so the runtime bound's custody check holds them to its
     * released lovelace plus their allocation.
     */
    assertComplete: (): void => {
      if (bonds !== 1 || cases !== 1)
        throw new Error(
          `validation dispute VerifySource must pay exactly one bond and one case output: bonds=${bonds.toString()} cases=${cases.toString()}`,
        );
    },
  });
};

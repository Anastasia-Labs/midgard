/**
 * The validation-trace dispute's case-close funding authority.
 *
 * The close of our undefended case spends the case and its bond, which the
 * VerifySource that created them left, whoever ran it: when another party
 * verified our claim, neither has confirmed lineage in this workflow, so the
 * runtime funding bound refuses them without this authority. The authority
 * binds the close to our case as the thread moves' authority does: the case
 * our Open opened (the pending claim our Open left, read back from this
 * reservation's lineage, for the actuation's header, with the funding key
 * its fraud prover). It admits exactly that case's token and its bond's
 * marker, each at the exact output the builder spent, as released custody;
 * the close's payouts are case-v1's, which its validator fixes. It binds the
 * exact signed bytes evaluated locally and is minted only by the close
 * builder, for category 00000006.
 */
import { createHash } from "node:crypto";

import { computeDeploymentManifestJsonDigest } from "@al-ft/midgard-core/deployment-manifest-identity";
import * as SDK from "@al-ft/midgard-sdk";
import { CML, type TxSigned, type UTxO } from "@lucid-evolution/lucid";

import { assertWorkflowActuationPermitIdentity } from "./actuation-permit.js";
import {
  actionKind,
  exactResolvedOutputCbor,
} from "./funding-reservation-permit.create-workflow-funding-reservation-permit.js";
import {
  assertValidationDisputeMoveCase,
  failing,
  type Policy,
  type Terms,
} from "./funding-reservation-permit.validation-dispute-move-authority.js";
import type { PermitState } from "./funding-reservation-permit.workflow-funding-reservation-port.js";
import type { FraudProofWorkflowAction } from "./orchestrator.js";

const CATEGORY = "validationTraceDispute" as const;
const CLOSE = "close";

export type ValidationDisputeCloseAuthority = Readonly<{
  category: typeof CATEGORY;
  deploymentFingerprint: string;
  headerHash: string;
  /** The pending claim our Open left, in this reservation's lineage. */
  caseOpenerOutRef: string;
  caseOutRef: string;
  caseResolvedOutputCborHex: string;
  bondOutRef: string;
  bondResolvedOutputCborHex: string;
  transactionHash: string;
  transactionBodySha256: string;
  signedTransactionCborHex: string;
}>;

/** In this process only, keyed by the builder's own TxSigned, as VerifySource. */
const authorities = new WeakMap<TxSigned, ValidationDisputeCloseAuthority>();

const bodySha256 = (signed: TxSigned): string =>
  createHash("sha256")
    .update(Buffer.from(signed.toTransaction().body().to_cbor_hex(), "hex"))
    .digest("hex");

const outRefLabel = (utxo: Pick<UTxO, "txHash" | "outputIndex">) =>
  `${utxo.txHash}#${utxo.outputIndex.toString()}`;

/** Called by the close builder once it has evaluated and signed the close. */
export const grantValidationDisputeCloseAuthority = ({
  signed,
  deploymentFingerprint,
  headerHash,
  caseOpenerOutRef,
  caseInput,
  bond,
}: {
  readonly signed: TxSigned;
  readonly deploymentFingerprint: string;
  readonly headerHash: string;
  readonly caseOpenerOutRef: string;
  readonly caseInput: UTxO;
  readonly bond: UTxO;
}): void => {
  const transaction = signed.toTransaction();
  authorities.set(
    signed,
    Object.freeze({
      category: CATEGORY,
      deploymentFingerprint,
      headerHash,
      caseOpenerOutRef,
      caseOutRef: outRefLabel(caseInput),
      caseResolvedOutputCborHex: exactResolvedOutputCbor(caseInput),
      bondOutRef: outRefLabel(bond),
      bondResolvedOutputCborHex: exactResolvedOutputCbor(bond),
      transactionHash: CML.hash_transaction(transaction.body()).to_hex(),
      transactionBodySha256: bodySha256(signed),
      signedTransactionCborHex: transaction.to_cbor_hex(),
    }),
  );
};

export const readValidationDisputeCloseAuthority = (
  signed: TxSigned,
): ValidationDisputeCloseAuthority | null => {
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
      "signed validation dispute case close changed after local evaluation",
    );
  return authority;
};

export const VALIDATION_DISPUTE_CLOSE_BINDING =
  "validation dispute close authority differs from its exact case close or deployment terms";

const refuse = (names: readonly string[]): never => {
  throw new Error(`${VALIDATION_DISPUTE_CLOSE_BINDING} (${names.join(", ")})`);
};

/**
 * The authority's binding to this workflow, its action and the policy's
 * dispute terms. Each refusal names the failed binding.
 */
export const assertValidationDisputeCloseBinding = ({
  authority,
  policy,
  state,
  action,
  headerHash,
}: {
  readonly authority: ValidationDisputeCloseAuthority;
  readonly policy: Policy;
  readonly state: PermitState;
  readonly action: FraudProofWorkflowAction;
  /** The actuation permit's challenged header. */
  readonly headerHash: string;
}): Terms => {
  const terms = policy.validationDispute;
  const differing = failing([
    ["category", state.category === CATEGORY],
    ["policy category", policy.category === CATEGORY],
    ["dispute terms", terms !== undefined],
    ["current action kind", state.currentActionKind === CLOSE],
    ["action kind", actionKind(action) === CLOSE],
    [
      "action digest",
      state.currentActionDigest === computeDeploymentManifestJsonDigest(action),
    ],
    ["case", action.input.consumes === authority.caseOutRef],
    ["header hash", authority.headerHash === headerHash],
    [
      "deployment fingerprint",
      authority.deploymentFingerprint === policy.deploymentFingerprint,
    ],
  ]);
  if (differing.length > 0 || terms === undefined) refuse(differing);
  return terms!;
};

/** Our case, as this reservation's lineage holds the claim our Open left. */
export const assertValidationDisputeCloseCase = (input: {
  readonly authority: ValidationDisputeCloseAuthority;
  readonly lineage: unknown;
  readonly fundingPaymentKeyHash: string;
}): string => assertValidationDisputeMoveCase({ ...input, refuse });

/**
 * The spent case and bond: the authority's exact outputs, holding our
 * case's token and its bond's marker.
 */
export const assertValidationDisputeCloseInputs = ({
  authority,
  terms,
  caseId,
  caseInput,
  bond,
}: {
  readonly authority: ValidationDisputeCloseAuthority;
  readonly terms: Terms;
  readonly caseId: string;
  readonly caseInput: UTxO;
  readonly bond: UTxO;
}): void => {
  const differing = failing([
    [
      "case input",
      exactResolvedOutputCbor(caseInput) ===
        authority.caseResolvedOutputCborHex,
    ],
    ["case input token", caseInput.assets[terms.casePolicyId + caseId] === 1n],
    [
      "bond input",
      exactResolvedOutputCbor(bond) === authority.bondResolvedOutputCborHex,
    ],
    [
      "bond input marker",
      bond.assets[
        terms.casePolicyId + SDK.validationCaseBondMarkerName(caseId)
      ] === 1n,
    ],
  ]);
  if (differing.length > 0) refuse(differing);
};

/**
 * The case close's funding under the authority its signed transaction
 * carries, checked step by step as the runtime bound walks it.
 */
export const validationDisputeCloseFunding = async ({
  authority,
  slash,
  policy,
  state,
  action,
}: {
  readonly authority: ValidationDisputeCloseAuthority;
  readonly slash: boolean;
  readonly policy: Policy;
  readonly state: PermitState;
  readonly action: FraudProofWorkflowAction;
}) => {
  if (slash)
    throw new Error(
      "funding transaction carries both a fraud slash and a validation dispute close authority",
    );
  const terms = assertValidationDisputeCloseBinding({
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
  const caseId = assertValidationDisputeCloseCase({
    authority,
    lineage: await state.port.resolveConfirmedInput({
      outRef: authority.caseOpenerOutRef,
    }),
    fundingPaymentKeyHash: policy.fundingPaymentKeyHash,
  });
  return Object.freeze({
    /** Finds the spent case and bond and checks them. */
    admitOpener: (inputs: ReadonlyMap<string, UTxO>): void => {
      const caseInput = inputs.get(authority.caseOutRef);
      const bond = inputs.get(authority.bondOutRef);
      if (caseInput === undefined || bond === undefined)
        throw new Error(
          "validation dispute case close omits its case or its bond",
        );
      assertValidationDisputeCloseInputs({
        authority,
        terms,
        caseId,
        caseInput,
        bond,
      });
    },
    /** The case and its bond as released custody; nothing else. */
    admitContractInput: (outRef: string, input: UTxO): unknown => {
      if (outRef !== authority.caseOutRef && outRef !== authority.bondOutRef)
        throw new Error(
          "validation dispute case close consumes a contract input besides its case and bond",
        );
      return {
        sourceActionKind: CLOSE,
        sourceOutputIndex: input.outputIndex,
        outRef,
        resolvedOutputCborHex: exactResolvedOutputCbor(input),
      };
    },
    /** The bound's own accounting holds the close's outputs. */
    output: (_raw: CML.TransactionOutput): null => null,
    /** The record and the locked case tokens are custody the case released. */
    refuseOtherCustody: (): void => undefined,
    refuseProtocolTopup: (protocolTopup: bigint): void => {
      if (protocolTopup !== 0n)
        throw new Error(
          `validation dispute case close tops up protocol state: topup=${protocolTopup.toString()}`,
        );
    },
    assertComplete: (): void => undefined,
  });
};

/**
 * The validation-trace dispute's thread-move funding authority.
 *
 * The challenger's moves on a defended case spend a thread the defender
 * created, so the thread has no confirmed lineage in this workflow and the
 * runtime funding bound refuses it without this authority. The authority
 * admits exactly one thread input: one token of the case policy, named from
 * our own confirmed case (the case output our VerifySource left, read back
 * from this reservation's lineage) and the defender key in the thread's
 * datum, with the funding wallet as its fraud prover. A reveal or timeout
 * pays one successor, the thread less the fee at the game or timeout
 * validator; a thread close pays the case back unchanged but for its datum,
 * the defender's lost-defense receipt at the award validator, and the rest
 * of the thread less the receipt and the fee to the funding key, as case-v1
 * requires. Nothing is topped up; the fee is at most the deployment's
 * per-move cap; the wallet lends only collateral. It binds the exact signed
 * bytes evaluated locally and is minted only by the move builders, for
 * category 00000006. The funding walk under it is in
 * funding-reservation-permit.assert-validation-dispute-move.ts.
 */
import { createHash } from "node:crypto";

import { computeDeploymentManifestJsonDigest } from "@al-ft/midgard-core/deployment-manifest-identity";
import * as SDK from "@al-ft/midgard-sdk";
import {
  CML,
  coreToTxOutput,
  Data,
  type TxOutput,
  type TxSigned,
  type UTxO,
} from "@lucid-evolution/lucid";
import { Effect } from "effect";

import {
  actionKind,
  exactResolvedOutputCbor,
  parseConfirmedActionOutput,
} from "./funding-reservation-permit.create-workflow-funding-reservation-permit.js";
import type { PermitState } from "./funding-reservation-permit.workflow-funding-reservation-port.js";
import type { FraudProofWorkflowAction } from "./orchestrator.js";
import type { readWorkflowRuntimeFundingPolicy } from "./runtime-funding-policy.js";

const CATEGORY = "validationTraceDispute" as const;
const VERIFY_SOURCE = "verify_source";

export type ValidationDisputeMoveKind = "reveal" | "timeout" | "close_thread";

export type ValidationDisputeMoveAuthority = Readonly<{
  category: typeof CATEGORY;
  kind: ValidationDisputeMoveKind;
  deploymentFingerprint: string;
  headerHash: string;
  /** The case output our VerifySource left, in this reservation's lineage. */
  caseOriginOutRef: string;
  threadOutRef: string;
  threadResolvedOutputCborHex: string;
  /** A thread close's case input; null for a reveal or timeout. */
  caseOutRef: string | null;
  caseResolvedOutputCborHex: string | null;
  transactionHash: string;
  transactionBodySha256: string;
  signedTransactionCborHex: string;
}>;

export type Policy = ReturnType<typeof readWorkflowRuntimeFundingPolicy>;
export type Terms = NonNullable<Policy["validationDispute"]>;
export type Moves = NonNullable<Terms["moves"]>;

/** In this process only, keyed by the builder's own TxSigned, as VerifySource. */
const authorities = new WeakMap<TxSigned, ValidationDisputeMoveAuthority>();

const bodySha256 = (signed: TxSigned): string =>
  createHash("sha256")
    .update(Buffer.from(signed.toTransaction().body().to_cbor_hex(), "hex"))
    .digest("hex");

const outRefLabel = (utxo: Pick<UTxO, "txHash" | "outputIndex">) =>
  `${utxo.txHash}#${utxo.outputIndex.toString()}`;

/** Called by a move builder once it has evaluated and signed its move. */
export const grantValidationDisputeMoveAuthority = ({
  signed,
  kind,
  deploymentFingerprint,
  headerHash,
  caseOriginOutRef,
  thread,
  caseInput,
}: {
  readonly signed: TxSigned;
  readonly kind: ValidationDisputeMoveKind;
  readonly deploymentFingerprint: string;
  readonly headerHash: string;
  readonly caseOriginOutRef: string;
  readonly thread: UTxO;
  readonly caseInput?: UTxO;
}): void => {
  const transaction = signed.toTransaction();
  authorities.set(
    signed,
    Object.freeze({
      category: CATEGORY,
      kind,
      deploymentFingerprint,
      headerHash,
      caseOriginOutRef,
      threadOutRef: outRefLabel(thread),
      threadResolvedOutputCborHex: exactResolvedOutputCbor(thread),
      caseOutRef: caseInput === undefined ? null : outRefLabel(caseInput),
      caseResolvedOutputCborHex:
        caseInput === undefined ? null : exactResolvedOutputCbor(caseInput),
      transactionHash: CML.hash_transaction(transaction.body()).to_hex(),
      transactionBodySha256: bodySha256(signed),
      signedTransactionCborHex: transaction.to_cbor_hex(),
    }),
  );
};

export const readValidationDisputeMoveAuthority = (
  signed: TxSigned,
): ValidationDisputeMoveAuthority | null => {
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
      "signed validation dispute move changed after local evaluation",
    );
  return authority;
};

export const VALIDATION_DISPUTE_MOVE_BINDING =
  "validation dispute move authority differs from its exact thread move or deployment terms";

export const refuse = (names: readonly string[]): never => {
  throw new Error(`${VALIDATION_DISPUTE_MOVE_BINDING} (${names.join(", ")})`);
};

const failing = (rows: readonly (readonly [string, boolean])[]) =>
  rows.filter(([, holds]) => !holds).map(([name]) => name);

/**
 * The authority's binding to this workflow, its action and the policy's
 * dispute move terms. Each refusal names the failed binding.
 */
export const assertValidationDisputeMoveBinding = ({
  authority,
  policy,
  state,
  action,
  headerHash,
}: {
  readonly authority: ValidationDisputeMoveAuthority;
  readonly policy: Policy;
  readonly state: PermitState;
  readonly action: FraudProofWorkflowAction;
  /** The actuation permit's challenged header. */
  readonly headerHash: string;
}): Readonly<{ terms: Terms; moves: Moves }> => {
  const terms = policy.validationDispute;
  const differing = failing([
    ["category", state.category === CATEGORY],
    ["policy category", policy.category === CATEGORY],
    ["move terms", terms?.moves !== undefined],
    ["current action kind", state.currentActionKind === authority.kind],
    ["action kind", actionKind(action) === authority.kind],
    [
      "action digest",
      state.currentActionDigest === computeDeploymentManifestJsonDigest(action),
    ],
    ["thread", action.input.consumes === authority.threadOutRef],
    ["header hash", authority.headerHash === headerHash],
    [
      "deployment fingerprint",
      authority.deploymentFingerprint === policy.deploymentFingerprint,
    ],
  ]);
  if (differing.length > 0 || terms?.moves === undefined) refuse(differing);
  return { terms: terms!, moves: terms!.moves! };
};

const caseDatumOf = (output: TxOutput): SDK.Case | null => {
  try {
    return Data.from(output.datum!, SDK.Case);
  } catch {
    return null;
  }
};

/**
 * Our case, as this reservation's lineage holds the output our VerifySource
 * left: at the dispute case address, with exactly one case token, for the
 * actuation's header, the funding key its bounty claimant. Returns its id.
 */
export const assertValidationDisputeMoveCase = ({
  authority,
  terms,
  lineage,
  fundingPaymentKeyHash,
}: {
  readonly authority: ValidationDisputeMoveAuthority;
  readonly terms: Terms;
  /** The reservation's confirmed lineage of the case origin, or null. */
  readonly lineage: unknown;
  readonly fundingPaymentKeyHash: string;
}): string => {
  const confirmed =
    lineage === null ? null : parseConfirmedActionOutput(lineage);
  if (
    confirmed === null ||
    confirmed.sourceActionKind !== VERIFY_SOURCE ||
    confirmed.outRef !== authority.caseOriginOutRef
  )
    refuse(["case origin lineage"]);
  const output = coreToTxOutput(
    CML.TransactionOutput.from_cbor_hex(confirmed!.resolvedOutputCborHex),
  );
  const tokens = Object.entries(output.assets).filter(([unit]) =>
    unit.startsWith(terms.casePolicyId),
  );
  const caseDatum = caseDatumOf(output);
  const differing = failing([
    ["case origin address", output.address === terms.caseAddress],
    ["case origin token", tokens.length === 1 && tokens[0]![1] === 1n],
    [
      "case origin header",
      caseDatum?.block_header_hash === authority.headerHash,
    ],
    [
      "case origin claimant",
      caseDatum?.bounty_claimant === fundingPaymentKeyHash,
    ],
  ]);
  if (differing.length > 0) refuse(differing);
  return tokens[0]![0].slice(56);
};

/** hash(case id ‖ defender key hash): a thread token's and receipt's name. */
const threadName = (caseId: string, defenderVkh: string): string =>
  Effect.runSync(SDK.hashHexWithBlake2b(caseId + defenderVkh, 32));

export const exactAssets = (
  assets: Readonly<Record<string, bigint>>,
  expected: Readonly<Record<string, bigint>>,
): boolean => {
  const units = Object.keys(assets);
  return (
    units.length === Object.keys(expected).length &&
    units.every((unit) => assets[unit] === expected[unit])
  );
};

/**
 * The spent thread: the authority's exact output, at the game (a reveal or
 * timeout) or the timeout validator (a close), holding its lovelace and only
 * the case policy's token named from our case and the defender key in its
 * datum, with the funding key its fraud prover. Returns its token's name.
 */
export const assertValidationDisputeMoveThread = ({
  authority,
  terms,
  moves,
  caseId,
  thread,
  fundingPaymentKeyHash,
}: {
  readonly authority: ValidationDisputeMoveAuthority;
  readonly terms: Terms;
  readonly moves: Moves;
  readonly caseId: string;
  readonly thread: UTxO;
  readonly fundingPaymentKeyHash: string;
}): string => {
  let datum: SDK.CaseThreadDatum | null = null;
  try {
    datum = Data.from(thread.datum!, SDK.CaseThreadDatum);
  } catch {
    refuse(["thread datum"]);
  }
  const name = threadName(caseId, datum!.data?.header.defender_vkh ?? "");
  const differing = failing([
    [
      "thread output",
      exactResolvedOutputCbor(thread) === authority.threadResolvedOutputCborHex,
    ],
    [
      "thread address",
      thread.address ===
        (authority.kind === "close_thread"
          ? moves.timeoutAddress
          : moves.gameAddress),
    ],
    [
      "thread token",
      exactAssets(thread.assets, {
        lovelace: thread.assets.lovelace ?? 0n,
        [terms.casePolicyId + name]: 1n,
      }),
    ],
    ["thread prover", datum!.fraud_prover === fundingPaymentKeyHash],
    ["thread case", datum!.data?.case_id === caseId],
  ]);
  if (differing.length > 0) refuse(differing);
  return name;
};

/** A thread close's case input: the authority's, at our case, by its token. */
export const assertValidationDisputeMoveCaseInput = ({
  authority,
  terms,
  caseId,
  caseInput,
}: {
  readonly authority: ValidationDisputeMoveAuthority;
  readonly terms: Terms;
  readonly caseId: string;
  readonly caseInput: UTxO;
}): void => {
  const differing = failing([
    [
      "case input",
      exactResolvedOutputCbor(caseInput) ===
        authority.caseResolvedOutputCborHex,
    ],
    ["case input address", caseInput.address === terms.caseAddress],
    ["case input token", caseInput.assets[terms.casePolicyId + caseId] === 1n],
  ]);
  if (differing.length > 0) refuse(differing);
};

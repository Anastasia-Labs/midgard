/**
 * A validation-trace dispute thread move's funding, walked by the runtime
 * bound under its authority (funding-reservation-permit.validation-dispute-
 * move-authority.ts): the exact outputs game-v1 and case-v1 fix, the mint a
 * thread close makes, and nothing topped up.
 */
import {
  CML,
  coreToTxOutput,
  getAddressDetails,
  type TxSigned,
  type UTxO,
} from "@lucid-evolution/lucid";

import { assertWorkflowActuationPermitIdentity } from "./actuation-permit.js";
import { exactResolvedOutputCbor } from "./funding-reservation-permit.create-workflow-funding-reservation-permit.js";
import { addMintedAssets } from "./funding-reservation-permit.ledger-accounting.js";
import {
  assertValidationDisputeMoveBinding,
  assertValidationDisputeMoveCase,
  assertValidationDisputeMoveCaseInput,
  assertValidationDisputeMoveThread,
  exactAssets,
  type Moves,
  type Policy,
  readValidationDisputeMoveAuthority,
  refuse,
  type Terms,
  type ValidationDisputeMoveAuthority,
} from "./funding-reservation-permit.validation-dispute-move-authority.js";
import type { PermitState } from "./funding-reservation-permit.workflow-funding-reservation-port.js";
import type { FraudProofWorkflowAction } from "./orchestrator.js";

export {
  grantValidationDisputeMoveAuthority,
  readValidationDisputeMoveAuthority,
  VALIDATION_DISPUTE_MOVE_BINDING,
} from "./funding-reservation-permit.validation-dispute-move-authority.js";

type Kept = "successor" | "case" | "receipt" | "leftover";

/**
 * Classifies a body output under the authority; an output at a dispute
 * address the move pays must be exactly the one case-v1 and game-v1 fix.
 */
export const validationDisputeMoveOutput = ({
  authority,
  moves,
  terms,
  raw,
  thread,
  name,
  caseInput,
  fee,
  fundingPaymentKeyHash,
}: {
  readonly authority: ValidationDisputeMoveAuthority;
  readonly moves: Moves;
  readonly terms: Terms;
  readonly raw: CML.TransactionOutput;
  readonly thread: UTxO;
  readonly name: string;
  readonly caseInput: UTxO | undefined;
  readonly fee: bigint;
  readonly fundingPaymentKeyHash: string;
}): Kept | null => {
  const output = coreToTxOutput(raw);
  const inline = output.datum != null && output.scriptRef == null;
  const threadLovelace = thread.assets.lovelace ?? 0n;
  const fail = (what: string): never => {
    throw new Error(`validation dispute ${what}`);
  };
  if (authority.kind !== "close_thread") {
    const at =
      authority.kind === "reveal" ? moves.gameAddress : moves.timeoutAddress;
    if (output.address !== at) return null;
    if (
      !inline ||
      !exactAssets(output.assets, {
        lovelace: threadLovelace - fee,
        [terms.casePolicyId + name]: 1n,
      })
    )
      fail("move successor differs from its thread less the fee");
    return "successor";
  }
  if (output.address === terms.caseAddress) {
    if (!inline || !exactAssets(output.assets, caseInput!.assets))
      fail("thread close changes the case's value");
    return "case";
  }
  if (output.address === moves.awardAddress) {
    if (
      !inline ||
      !exactAssets(output.assets, {
        lovelace: BigInt(moves.fraudProofOutputLovelace),
        [moves.awardPolicyId + name]: 1n,
      })
    )
      fail("thread close receipt differs from its exact receipt");
    return "receipt";
  }
  const details = getAddressDetails(output.address);
  if (
    details.type === "Enterprise" &&
    details.paymentCredential?.type === "Key" &&
    details.paymentCredential.hash === fundingPaymentKeyHash
  ) {
    if (
      output.scriptRef != null ||
      !exactAssets(output.assets, {
        lovelace: threadLovelace - BigInt(moves.fraudProofOutputLovelace) - fee,
      })
    )
      fail(
        "thread close leftover differs from its thread less receipt and fee",
      );
    return "leftover";
  }
  return null;
};

/**
 * A thread move's funding under its authority, checked step by step as the
 * runtime bound walks the signed transaction; `null` when it carries none.
 */
export const validationDisputeMoveFunding = async ({
  signed,
  slash,
  policy,
  state,
  action,
}: {
  readonly signed: TxSigned;
  readonly slash: boolean;
  readonly policy: Policy;
  readonly state: PermitState;
  readonly action: FraudProofWorkflowAction;
}) => {
  const authority = readValidationDisputeMoveAuthority(signed);
  if (authority === null) return null;
  if (slash)
    throw new Error(
      "funding transaction carries both a fraud slash and a validation dispute move authority",
    );
  const { terms, moves } = assertValidationDisputeMoveBinding({
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
  const fundingPaymentKeyHash = policy.fundingPaymentKeyHash;
  const caseId = assertValidationDisputeMoveCase({
    authority,
    terms,
    lineage: await state.port.resolveConfirmedInput({
      outRef: authority.caseOriginOutRef,
    }),
    fundingPaymentKeyHash,
  });
  const body = signed.toTransaction().body();
  const fee = body.fee();
  if (fee > BigInt(moves.moveFeeCapLovelace)) refuse(["fee"]);
  const close = authority.kind === "close_thread";
  let thread: UTxO | undefined;
  let caseInput: UTxO | undefined;
  let name = "";
  const kept: Kept[] = [];
  return Object.freeze({
    /** Finds the spent thread (and a close's case) and checks them. */
    admitOpener: (inputs: ReadonlyMap<string, UTxO>): void => {
      thread = inputs.get(authority.threadOutRef);
      if (thread === undefined)
        throw new Error("validation dispute move omits its thread");
      name = assertValidationDisputeMoveThread({
        authority,
        terms,
        moves,
        caseId,
        thread,
        fundingPaymentKeyHash,
      });
      if (!close) return;
      caseInput = inputs.get(authority.caseOutRef ?? "");
      if (caseInput === undefined)
        throw new Error("validation dispute thread close omits its case");
      assertValidationDisputeMoveCaseInput({
        authority,
        terms,
        caseId,
        caseInput,
      });
    },
    /** The thread (and a close's case) as released custody; nothing else. */
    admitContractInput: (outRef: string, input: UTxO): unknown => {
      if (
        outRef !== authority.threadOutRef &&
        !(close && outRef === authority.caseOutRef)
      )
        throw new Error(
          "validation dispute move consumes a contract input besides its thread and case",
        );
      return {
        sourceActionKind: authority.kind,
        sourceOutputIndex: input.outputIndex,
        outRef,
        resolvedOutputCborHex: exactResolvedOutputCbor(input),
      };
    },
    /** A kept output's custody; the leftover leaves custody. */
    output: (
      raw: CML.TransactionOutput,
    ): Readonly<{ custody: bigint; allocation: bigint }> | null => {
      const kind = validationDisputeMoveOutput({
        authority,
        moves,
        terms,
        raw,
        thread: thread!,
        name,
        caseInput,
        fee,
        fundingPaymentKeyHash,
      });
      if (kind === null) return null;
      kept.push(kind);
      return {
        custody: kind === "leftover" ? 0n : raw.amount().coin(),
        allocation: 0n,
      };
    },
    refuseOtherCustody: (): never => {
      throw new Error(
        "validation dispute move pays custody beyond its thread's successors",
      );
    },
    refuseProtocolTopup: (protocolTopup: bigint): void => {
      if (protocolTopup !== 0n)
        throw new Error(
          `validation dispute move tops up protocol state: topup=${protocolTopup.toString()}`,
        );
    },
    /** Exactly the outputs and mint the move's validators fix. */
    assertComplete: (): void => {
      const expected: readonly Kept[] = close
        ? ["case", "leftover", "receipt"]
        : ["successor"];
      if ([...kept].sort().join() !== expected.join())
        throw new Error(
          `validation dispute ${authority.kind} must pay exactly ${expected.join(", ")}: paid=${kept.join(", ")}`,
        );
      const minted = new Map<string, bigint>();
      addMintedAssets(minted, body.mint());
      if (
        !exactAssets(
          Object.fromEntries(minted),
          close
            ? {
                [terms.casePolicyId + name]: -1n,
                [moves.awardPolicyId + name]: 1n,
              }
            : {},
        )
      )
        throw new Error(
          `validation dispute ${authority.kind} mints other than its thread's own tokens`,
        );
    },
  });
};

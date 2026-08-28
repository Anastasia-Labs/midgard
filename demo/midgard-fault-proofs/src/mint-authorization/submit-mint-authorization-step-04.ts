/**
 * `mint-authorization` step-04 submitters — the direction-A
 * reference-input scan.
 *
 * - `submitMintAuthorizationStep04ResolveNext` — the self-loop. Opens the
 *   committed field 1 at the thread's cursor, resolves the outpoint under
 *   the pre-state ledger root (MPF membership of the descriptor), and lets
 *   the validator prove the resolved output carries no reference script of
 *   the claimed policy hash. The thread returns to step-04's own address
 *   with the cursor advanced.
 * - `submitMintAuthorizationStep04AdvanceComplete` — closes the scan once
 *   the cursor equals the authenticated field-1 item count; pays the
 *   direction-A verdict forward to step-05.
 *
 * Local refusals mirror the validator: a cursor outside the committed
 * field, a descriptor that DOES anchor the claimed policy as a reference
 * script, a trie whose root is not the thread's `prior_ledger_root`, an
 * early completion.
 */
import {
  decodeMidgardFieldPreimageV1,
  decodeMidgardLedgerOutputCommitmentV1,
  decodeMidgardSpendInputItemV1,
} from "@al-ft/midgard-core";
import type {
  MintAuthorizationStep04Args,
  MintAuthorizationStep04StateV1,
  MintAuthorizationStep05StateV1,
} from "@al-ft/midgard-sdk";
import {
  fieldOpeningV1ForField,
  MIDGARD_FIELD_INDEX_V1,
  MINT_AUTHORIZATION_DIRECTION_SCRIPT_ABSENT_V1,
  MintAuthorizationStep04Datum,
  MintAuthorizationStep04SpendRedeemer,
  MintAuthorizationStep05Datum,
  requireInputIndex,
  requireOwnSpendPurpose,
  requireUniqueOutputIndex,
} from "@al-ft/midgard-sdk";
import {
  type BuildTxWithRedeemer,
  Data,
  type LucidEvolution,
  type UTxO,
} from "@lucid-evolution/lucid";

import {
  DEFAULT_CONFIRMATION_POLL_MS,
  type ResolvedProverSigner,
} from "../runtime.js";
import { selectFeeInput } from "../submit-step-01.js";
import { computationThreadOutputPredicate } from "../tx-layout.js";
import type { MintAuthorizationContractsV1 } from "./contracts-v1.js";
import {
  buildMintAuthorizationLedgerMembershipV1,
  type MintAuthorizationLedgerTrieHandleV1,
} from "./evidence-v1.js";
import {
  mintAuthorizationStepLabelV1,
  mintAuthorizationSubmitError,
  requireMintAuthorizationReferenceScriptV1,
  requireMintAuthorizationStepStateV1,
  requireMintAuthorizationThreadUtxoV1,
} from "./submit-common-v1.js";

const STEP_LABEL = mintAuthorizationStepLabelV1(3);

export type SubmitMintAuthorizationStep04Result = {
  readonly txHash: string;
  readonly walletSource: string;
  readonly proverAddress: string;
  readonly fraudProver: string;
  readonly threadOutRef: string;
  readonly nextThreadOutRef: string;
  readonly fraudulentHeaderHash: string;
  readonly computationThreadUnit: string;
  readonly nextStepAddress: string;
  /** The cursor the thread carries after this transaction (ResolveNext). */
  readonly nextRefCursor: bigint | null;
  readonly inputIndex: number;
  readonly outputIndex: number;
  readonly awaitedConfirmation: boolean;
};

type Step04Shared = {
  readonly lucid: LucidEvolution;
  readonly contracts: MintAuthorizationContractsV1;
  readonly categoryId: string;
  readonly signer: ResolvedProverSigner;
  readonly threadOutRef: string;
  /** The bound transaction's compact CBOR, hex. */
  readonly nativeTxCompactCbor: string;
  /** The committed field-1 preimage bytes, hex — the Inline carriage. */
  readonly referenceInputsPreimageCborHex: string;
  /** The published step-04 reference script; inline-attached when absent. */
  readonly referenceScriptUtxo?: UTxO;
  readonly awaitConfirmation?: boolean;
};

const prepareStep04 = async (shared: Step04Shared) => {
  const { threadUtxo, threadToken } =
    await requireMintAuthorizationThreadUtxoV1({
      lucid: shared.lucid,
      contracts: shared.contracts,
      categoryId: shared.categoryId,
      stepIndex: 3,
      threadOutRef: shared.threadOutRef,
    });
  const state: MintAuthorizationStep04StateV1 =
    requireMintAuthorizationStepStateV1({
      threadUtxo,
      signer: shared.signer,
      schema: MintAuthorizationStep04Datum,
      stepIndex: 3,
    });
  const referenceItems = decodeMidgardFieldPreimageV1(
    Buffer.from(shared.referenceInputsPreimageCborHex, "hex"),
  );
  const opening = fieldOpeningV1ForField({
    fieldIndex: MIDGARD_FIELD_INDEX_V1.referenceInputs,
    nativeTxCompactCbor: shared.nativeTxCompactCbor,
    carriage: { Inline: { preimage: shared.referenceInputsPreimageCborHex } },
  });
  return { threadUtxo, threadToken, state, referenceItems, opening };
};

const submitPreparedStep04 = async ({
  shared,
  threadUtxo,
  threadToken,
  nextStepIndex,
  nextStepDatum,
  nextRefCursor,
  argsOf,
}: {
  readonly shared: Step04Shared;
  readonly threadUtxo: UTxO;
  readonly threadToken: {
    readonly unit: string;
    readonly fraudulentHeaderHash: string;
  };
  /** 3 = the self-loop back to step-04's own address, 4 = step-05. */
  readonly nextStepIndex: 3 | 4;
  readonly nextStepDatum: string;
  readonly nextRefCursor: bigint | null;
  readonly argsOf: (layout: {
    readonly inputIndex: bigint;
    readonly outputIndex: bigint;
  }) => MintAuthorizationStep04Args;
}): Promise<SubmitMintAuthorizationStep04Result> => {
  const { lucid, contracts, signer, referenceScriptUtxo } = shared;
  const awaitConfirmation = shared.awaitConfirmation ?? true;
  signer.selectWallet(lucid);
  const feeInput = selectFeeInput(await lucid.wallet().getUtxos());
  const nextOutputMatches = computationThreadOutputPredicate({
    address: contracts.steps[nextStepIndex].spendingScriptAddress,
    datum: nextStepDatum,
    unit: threadToken.unit,
  });
  let resolvedLayout:
    | { readonly inputIndex: bigint; readonly outputIndex: bigint }
    | undefined;
  const redeemer = ((ctx) => {
    requireOwnSpendPurpose(ctx, threadUtxo, STEP_LABEL);
    const layout = {
      inputIndex: requireInputIndex(ctx, threadUtxo, STEP_LABEL),
      outputIndex: requireUniqueOutputIndex(
        ctx.outputs,
        nextOutputMatches,
        `${STEP_LABEL} output`,
      ),
    };
    resolvedLayout = layout;
    return Data.to(
      { Continue: [argsOf(layout)] },
      MintAuthorizationStep04SpendRedeemer,
    );
  }) satisfies BuildTxWithRedeemer;
  const threadAssets = {
    lovelace: threadUtxo.assets.lovelace ?? 0n,
    [threadToken.unit]: 1n,
  };

  const base = lucid
    .newTx()
    .collectFrom([feeInput])
    .collectFrom([threadUtxo], redeemer)
    .pay.ToContract(
      contracts.steps[nextStepIndex].spendingScriptAddress,
      { kind: "inline", value: nextStepDatum },
      threadAssets,
    )
    .addSignerKey(signer.paymentKeyHash);
  const tx =
    referenceScriptUtxo === undefined
      ? base.attach.SpendingValidator(contracts.steps[3].spendingScript)
      : base.readFrom([
          requireMintAuthorizationReferenceScriptV1({
            utxo: referenceScriptUtxo,
            expectedScriptHash: contracts.steps[3].spendingScriptHash,
            stepIndex: 3,
          }),
        ]);

  const unsigned = await tx.complete({ localUPLCEval: true });
  if (resolvedLayout === undefined) {
    throw mintAuthorizationSubmitError(
      "BuildTxWithRedeemer did not resolve the step-04 layout.",
    );
  }
  const signed = await unsigned.sign.withWallet().complete();
  const txHash = await signed.submit();
  if (awaitConfirmation) {
    await lucid.awaitTx(txHash, DEFAULT_CONFIRMATION_POLL_MS);
  }

  return {
    txHash,
    walletSource: signer.source,
    proverAddress: signer.address,
    fraudProver: signer.paymentKeyHash,
    threadOutRef: shared.threadOutRef,
    nextThreadOutRef: `${txHash}#${resolvedLayout.outputIndex.toString()}`,
    fraudulentHeaderHash: threadToken.fraudulentHeaderHash,
    computationThreadUnit: threadToken.unit,
    nextStepAddress: contracts.steps[nextStepIndex].spendingScriptAddress,
    nextRefCursor,
    inputIndex: Number(resolvedLayout.inputIndex),
    outputIndex: Number(resolvedLayout.outputIndex),
    awaitedConfirmation: awaitConfirmation,
  };
};

/** The self-loop: clear the reference input at the thread's cursor. */
export const submitMintAuthorizationStep04ResolveNext = async ({
  trie,
  descriptorCborHex,
  ...shared
}: Step04Shared & {
  /** Pre-state ledger trie handle; its root must be `prior_ledger_root`. */
  readonly trie: MintAuthorizationLedgerTrieHandleV1;
  /** The scanned outpoint's ledger descriptor bytes, hex (the MPF value). */
  readonly descriptorCborHex: string;
}): Promise<SubmitMintAuthorizationStep04Result> => {
  const { threadUtxo, threadToken, state, referenceItems, opening } =
    await prepareStep04(shared);
  const cursor = state.ref_cursor;
  if (cursor < 0n || cursor >= BigInt(referenceItems.length)) {
    throw mintAuthorizationSubmitError(
      `ref cursor ${cursor.toString()} is outside the committed field 1's ${referenceItems.length.toString()} items — ResolveNext has nothing to resolve.`,
    );
  }
  const outpointKey = Buffer.from(referenceItems[Number(cursor)]);
  // Local twin of the validator's canonical-item read (fail-closed).
  decodeMidgardSpendInputItemV1(outpointKey);
  const descriptor = decodeMidgardLedgerOutputCommitmentV1(
    Buffer.from(descriptorCborHex, "hex"),
  );
  if (
    descriptor.referenceScriptLanguage !== -1 &&
    descriptor.referenceScriptHash.toString("hex") === state.policy_id
  ) {
    throw mintAuthorizationSubmitError(
      `the resolved reference input at cursor ${cursor.toString()} carries a reference script hashing to the claimed policy ${state.policy_id} — the absence claim is false.`,
    );
  }
  const ledgerMembershipProof = await buildMintAuthorizationLedgerMembershipV1({
    trie,
    outpointKey,
    priorLedgerRootHex: state.prior_ledger_root,
  });

  const nextState: MintAuthorizationStep04StateV1 = {
    ...state,
    ref_cursor: cursor + 1n,
  };
  const nextStepDatum = Data.to(
    { fraud_prover: shared.signer.paymentKeyHash, data: nextState },
    MintAuthorizationStep04Datum,
  );
  return submitPreparedStep04({
    shared,
    threadUtxo,
    threadToken,
    nextStepIndex: 3,
    nextStepDatum,
    nextRefCursor: nextState.ref_cursor,
    argsOf: (layout) => ({
      ResolveNext: {
        input_index: layout.inputIndex,
        output_index: layout.outputIndex,
        reference_inputs_opening: opening,
        descriptor_cbor: descriptorCborHex,
        ledger_membership_proof: ledgerMembershipProof,
      },
    }),
  });
};

/** Close the completed scan into the direction-A verdict. */
export const submitMintAuthorizationStep04AdvanceComplete = async (
  shared: Step04Shared,
): Promise<SubmitMintAuthorizationStep04Result> => {
  const { threadUtxo, threadToken, state, referenceItems, opening } =
    await prepareStep04(shared);
  if (state.ref_cursor !== BigInt(referenceItems.length)) {
    throw mintAuthorizationSubmitError(
      `ref cursor ${state.ref_cursor.toString()} has not covered the committed field 1's ${referenceItems.length.toString()} items — the scan is incomplete.`,
    );
  }
  const step05State: MintAuthorizationStep05StateV1 = {
    policy_id: state.policy_id,
    direction: MINT_AUTHORIZATION_DIRECTION_SCRIPT_ABSENT_V1,
  };
  const nextStepDatum = Data.to(
    { fraud_prover: shared.signer.paymentKeyHash, data: step05State },
    MintAuthorizationStep05Datum,
  );
  return submitPreparedStep04({
    shared,
    threadUtxo,
    threadToken,
    nextStepIndex: 4,
    nextStepDatum,
    nextRefCursor: null,
    argsOf: (layout) => ({
      AdvanceComplete: {
        input_index: layout.inputIndex,
        output_index: layout.outputIndex,
        reference_inputs_opening: opening,
      },
    }),
  });
};

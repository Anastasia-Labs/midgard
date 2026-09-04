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
  decodeMidgardFieldPreimage,
  decodeMidgardLedgerOutputCommitment,
  decodeMidgardSpendInputItem,
} from "@al-ft/midgard-core";
import type {
  MintAuthorizationStep04Args,
  MintAuthorizationStep04State,
  MintAuthorizationStep05State,
} from "@al-ft/midgard-sdk";
import {
  MIDGARD_FIELD_INDEX,
  MINT_AUTHORIZATION_DIRECTION_SCRIPT_ABSENT,
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
  faultProofFieldOpening,
  planFaultProofFieldOpening,
  publishFaultProofFieldCarriage,
} from "../field-opening.js";
import {
  DEFAULT_CONFIRMATION_POLL_MS,
  type ResolvedProverSigner,
} from "../runtime.js";
import { excludeUtxo } from "../spend-input-witness.js";
import { selectFeeInput } from "../submit-step-01.js";
import { computationThreadOutputPredicate } from "../tx-layout.js";
import { witnessSpendingValidatorCarriage } from "../witness-reference-scripts.js";
import {
  type FraudProofPreSubmitBoundary,
  reachFraudProofPreSubmitBoundary,
  workflowReferenceScriptsUsedByTransaction,
} from "../workflow/transaction-boundary.js";
import type { MintAuthorizationContracts } from "./contracts.js";
import {
  buildMintAuthorizationLedgerMembership,
  type MintAuthorizationLedgerTrieHandle,
} from "./evidence.js";
import {
  mintAuthorizationStepLabel,
  mintAuthorizationSubmitError,
  requireMintAuthorizationReferenceScript,
  requireMintAuthorizationStepState,
  requireMintAuthorizationThreadUtxo,
} from "./submit-common.js";

const STEP_LABEL = mintAuthorizationStepLabel(3);

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
  readonly contracts: MintAuthorizationContracts;
  readonly categoryId: string;
  readonly signer: ResolvedProverSigner;
  readonly threadOutRef: string;
  /** The bound transaction's compact CBOR, hex. */
  readonly nativeTxCompactCbor: string;
  /**
   * The committed field-1 items — each §5.3 spend-input item's canonical
   * bytes, hex, in committed order. The §8 planner re-envelopes them and
   * picks the carriage tier from the resulting preimage's own byte length.
   */
  readonly referenceInputsItemCbors: readonly string[];
  /** Pre-minted §8.6 certificate when the planner selects tier 3. */
  readonly certificateUtxo?: UTxO;
  /** The mandatory published step-04 reference script. */
  readonly referenceScriptUtxo?: UTxO;
  readonly preSubmitBoundary?: FraudProofPreSubmitBoundary;
  readonly awaitConfirmation?: boolean;
};

const prepareStep04 = async (shared: Step04Shared) => {
  const { threadUtxo, threadToken } = await requireMintAuthorizationThreadUtxo({
    lucid: shared.lucid,
    contracts: shared.contracts,
    categoryId: shared.categoryId,
    stepIndex: 3,
    threadOutRef: shared.threadOutRef,
  });
  const state: MintAuthorizationStep04State = requireMintAuthorizationStepState(
    {
      threadUtxo,
      signer: shared.signer,
      schema: MintAuthorizationStep04Datum,
      stepIndex: 3,
    },
  );
  // The §8.8 door: plan field 1 against the bound tx id, then let the byte
  // length pick the tier — a scan over a large committed reference-input list
  // publishes tier-2 carriage rather than forcing it.
  const planned = planFaultProofFieldOpening({
    fieldIndex: MIDGARD_FIELD_INDEX.referenceInputs,
    anchorTxId: state.bad_tx_id,
    nativeTxCompactCbor: shared.nativeTxCompactCbor,
    itemCbors: shared.referenceInputsItemCbors.map((hex) =>
      Buffer.from(hex, "hex"),
    ),
    owner: shared.signer.paymentKeyHash,
    label: `${STEP_LABEL} reference-inputs`,
  });
  const referenceItems = decodeMidgardFieldPreimage(planned.preimage);
  shared.signer.selectWallet(shared.lucid);
  const carriageUtxos = await publishFaultProofFieldCarriage({
    lucid: shared.lucid,
    signer: shared.signer,
    planned,
    publisherAddress: shared.signer.address,
    label: `${STEP_LABEL} reference-inputs`,
  });
  const fieldReferenceInputs = [
    ...(shared.certificateUtxo === undefined ? [] : [shared.certificateUtxo]),
    ...carriageUtxos,
  ];
  return {
    threadUtxo,
    threadToken,
    state,
    referenceItems,
    planned,
    carriageUtxos,
    fieldReferenceInputs,
  };
};

const submitPreparedStep04 = async ({
  shared,
  threadUtxo,
  threadToken,
  planned,
  carriageUtxos,
  fieldReferenceInputs,
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
  readonly planned: ReturnType<typeof planFaultProofFieldOpening>;
  readonly carriageUtxos: readonly UTxO[];
  readonly fieldReferenceInputs: readonly UTxO[];
  /** 3 = the self-loop back to step-04's own address, 4 = step-05. */
  readonly nextStepIndex: 3 | 4;
  readonly nextStepDatum: string;
  readonly nextRefCursor: bigint | null;
  readonly argsOf: (
    layout: {
      readonly inputIndex: bigint;
      readonly outputIndex: bigint;
    },
    referenceInputsOpening: ReturnType<typeof faultProofFieldOpening>,
  ) => MintAuthorizationStep04Args;
}): Promise<SubmitMintAuthorizationStep04Result> => {
  const { lucid, contracts, signer, referenceScriptUtxo } = shared;
  const awaitConfirmation = shared.awaitConfirmation ?? true;
  signer.selectWallet(lucid);
  const stepReference =
    referenceScriptUtxo === undefined
      ? undefined
      : requireMintAuthorizationReferenceScript({
          utxo: referenceScriptUtxo,
          expectedScriptHash: contracts.steps[3].spendingScriptHash,
          stepIndex: 3,
        });
  const stepCarriage = witnessSpendingValidatorCarriage({
    script: contracts.steps[3].spendingScript,
    referenceUtxo: stepReference,
    label: `${STEP_LABEL} spending validator`,
  });
  // The §8.7 positional indices count into the transaction's COMPLETE
  // reference-input set, so the step's own reference script joins the field
  // carriage before the opening resolves.
  const referenceInputs = [
    ...fieldReferenceInputs,
    ...stepCarriage.referenceInputs,
  ];
  const referenceInputsOpening = faultProofFieldOpening({
    planned,
    referenceInputs,
    certificatePolicyId: contracts.fieldPreimageCertificatePolicyId,
    label: `${STEP_LABEL} reference-inputs`,
  });
  const walletUtxos = await lucid.wallet().getUtxos();
  const walletUtxosSansCarriage = carriageUtxos.reduce<readonly UTxO[]>(
    (candidates, utxo) => excludeUtxo(candidates, utxo),
    walletUtxos,
  );
  const feeInput = selectFeeInput(walletUtxosSansCarriage);
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
      { Continue: [argsOf(layout, referenceInputsOpening)] },
      MintAuthorizationStep04SpendRedeemer,
    );
  }) satisfies BuildTxWithRedeemer;
  const threadAssets = {
    lovelace: threadUtxo.assets.lovelace ?? 0n,
    [threadToken.unit]: 1n,
  };

  const withInputs = lucid
    .newTx()
    .collectFrom([feeInput])
    .collectFrom([threadUtxo], redeemer);
  const withReferences =
    referenceInputs.length === 0
      ? withInputs
      : withInputs.readFrom(referenceInputs);
  const paid = withReferences.pay
    .ToContract(
      contracts.steps[nextStepIndex].spendingScriptAddress,
      { kind: "inline", value: nextStepDatum },
      threadAssets,
    )
    .addSignerKey(signer.paymentKeyHash);
  const tx = stepCarriage.attach(paid);

  const unsigned = await tx.complete({
    localUPLCEval: true,
    ...(carriageUtxos.length === 0
      ? {}
      : { presetWalletInputs: walletUtxosSansCarriage as UTxO[] }),
  });
  if (resolvedLayout === undefined) {
    throw mintAuthorizationSubmitError(
      "BuildTxWithRedeemer did not resolve the step-04 layout.",
    );
  }
  const signed = await unsigned.sign.withWallet().complete();
  const expectedTxHash = await reachFraudProofPreSubmitBoundary({
    signed,
    referenceScripts: workflowReferenceScriptsUsedByTransaction({
      signed,
      candidates: [
        {
          role: "V1 fraud-proof mint-authorization step-04",
          utxo: referenceScriptUtxo,
          expectedScript: contracts.steps[3].spendingScript,
        },
      ],
    }),
    boundary: shared.preSubmitBoundary,
  });
  const txHash = await signed.submit();
  if (txHash !== expectedTxHash) {
    throw mintAuthorizationSubmitError(
      `step-04 provider returned ${txHash}, expected ${expectedTxHash}.`,
    );
  }
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
  readonly trie: MintAuthorizationLedgerTrieHandle;
  /** The scanned outpoint's ledger descriptor bytes, hex (the MPF value). */
  readonly descriptorCborHex: string;
}): Promise<SubmitMintAuthorizationStep04Result> => {
  const {
    threadUtxo,
    threadToken,
    state,
    referenceItems,
    planned,
    carriageUtxos,
    fieldReferenceInputs,
  } = await prepareStep04(shared);
  const cursor = state.ref_cursor;
  if (cursor < 0n || cursor >= BigInt(referenceItems.length)) {
    throw mintAuthorizationSubmitError(
      `ref cursor ${cursor.toString()} is outside the committed field 1's ${referenceItems.length.toString()} items — ResolveNext has nothing to resolve.`,
    );
  }
  const outpointKey = Buffer.from(referenceItems[Number(cursor)]);
  // Local twin of the validator's canonical-item read (fail-closed).
  decodeMidgardSpendInputItem(outpointKey);
  const descriptor = decodeMidgardLedgerOutputCommitment(
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
  const ledgerMembershipProof = await buildMintAuthorizationLedgerMembership({
    trie,
    outpointKey,
    priorLedgerRootHex: state.prior_ledger_root,
  });

  const nextState: MintAuthorizationStep04State = {
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
    planned,
    carriageUtxos,
    fieldReferenceInputs,
    nextStepIndex: 3,
    nextStepDatum,
    nextRefCursor: nextState.ref_cursor,
    argsOf: (layout, opening) => ({
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
  const {
    threadUtxo,
    threadToken,
    state,
    referenceItems,
    planned,
    carriageUtxos,
    fieldReferenceInputs,
  } = await prepareStep04(shared);
  if (state.ref_cursor !== BigInt(referenceItems.length)) {
    throw mintAuthorizationSubmitError(
      `ref cursor ${state.ref_cursor.toString()} has not covered the committed field 1's ${referenceItems.length.toString()} items — the scan is incomplete.`,
    );
  }
  const step05State: MintAuthorizationStep05State = {
    policy_id: state.policy_id,
    direction: MINT_AUTHORIZATION_DIRECTION_SCRIPT_ABSENT,
  };
  const nextStepDatum = Data.to(
    { fraud_prover: shared.signer.paymentKeyHash, data: step05State },
    MintAuthorizationStep05Datum,
  );
  return submitPreparedStep04({
    shared,
    threadUtxo,
    threadToken,
    planned,
    carriageUtxos,
    fieldReferenceInputs,
    nextStepIndex: 4,
    nextStepDatum,
    nextRefCursor: null,
    argsOf: (layout, opening) => ({
      AdvanceComplete: {
        input_index: layout.inputIndex,
        output_index: layout.outputIndex,
        reference_inputs_opening: opening,
      },
    }),
  });
};

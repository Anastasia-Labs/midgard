/**
 * `mint-authorization` step-04 submitters — the direction-A
 * resolved-input scan, over the reference inputs (field 1) and then the
 * spent inputs (field 0).
 *
 * - `submitMintAuthorizationStep04ResolveNext` — the self-loop. Opens the
 *   committed input field the thread names at its cursor, resolves the
 *   outpoint under the pre-state ledger root (MPF membership of the
 *   descriptor), and lets the validator prove the resolved output carries no
 *   reference script of the claimed policy hash. The thread returns to
 *   step-04's own address with the cursor advanced.
 * - `submitMintAuthorizationStep04AdvanceComplete` — once the cursor equals
 *   the field's authenticated item count, moves the scan from field 1 to
 *   field 0 at cursor 0, or, after field 0, pays the direction-A verdict
 *   forward to step-05.
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
import { selectFeeInput } from "../step-support.js";
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
import {
  step04ScanField,
  type SubmitMintAuthorizationStep04Result,
} from "./submit-mint-authorization-step-04.submit-mint-authorization-step04-scan-field.js";

export { type SubmitMintAuthorizationStep04Result } from "./submit-mint-authorization-step-04.submit-mint-authorization-step04-scan-field.js";

const STEP_LABEL = mintAuthorizationStepLabel(3);

type Step04Shared = {
  readonly lucid: LucidEvolution;
  readonly contracts: MintAuthorizationContracts;
  readonly categoryId: string;
  readonly signer: ResolvedProverSigner;
  readonly threadOutRef: string;
  /** The bound transaction's compact CBOR, hex. */
  readonly nativeTxCompactCbor: string;
  /**
   * The committed field-1 and field-0 items — each §5.3 spend-input item's
   * canonical bytes, hex, in committed order. The thread's `field_index`
   * selects one; the §8 planner re-envelopes it and picks the carriage tier
   * from the resulting preimage's own byte length.
   */
  readonly referenceInputsItemCbors: readonly string[];
  readonly spendInputsItemCbors: readonly string[];
  /** Pre-minted §8.6 certificate when the planner selects tier 3. */
  readonly certificateUtxo?: UTxO;
  /** Authenticated workflow publications; avoids publishing inside step capture. */
  readonly publishedCarriageUtxos?: readonly UTxO[];
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
  // The §8.8 door: plan the scanned input field against the bound tx id,
  // then let the byte length pick the tier — a scan over a large committed
  // input list publishes tier-2 carriage rather than forcing it.
  const scan = step04ScanField(shared, state);
  const planned = planFaultProofFieldOpening({
    anchorSourceKind: 0n,
    fieldIndex: scan.fieldIndex,
    anchorTxId: state.bad_tx_id,
    nativeTxCompactCbor: shared.nativeTxCompactCbor,
    itemCbors: scan.itemCbors.map((hex) => Buffer.from(hex, "hex")),
    owner: shared.signer.paymentKeyHash,
    label: `${STEP_LABEL} ${scan.label}`,
  });
  const scannedItems = decodeMidgardFieldPreimage(planned.preimage);
  shared.signer.selectWallet(shared.lucid);
  const carriageUtxos =
    shared.publishedCarriageUtxos ??
    (await publishFaultProofFieldCarriage({
      lucid: shared.lucid,
      signer: shared.signer,
      planned,
      publisherAddress: shared.signer.address,
      label: `${STEP_LABEL} ${scan.label}`,
    }));
  const fieldReferenceInputs = [
    ...(shared.certificateUtxo === undefined ? [] : [shared.certificateUtxo]),
    ...carriageUtxos,
  ];
  return {
    threadUtxo,
    threadToken,
    state,
    scan,
    scannedItems,
    planned,
    carriageUtxos,
    fieldReferenceInputs,
  };
};

const submitPreparedStep04 = async ({
  shared,
  threadUtxo,
  threadToken,
  scanLabel,
  planned,
  carriageUtxos,
  fieldReferenceInputs,
  nextStepIndex,
  nextStepDatum,
  nextScan,
  argsOf,
}: {
  readonly shared: Step04Shared;
  readonly threadUtxo: UTxO;
  readonly threadToken: {
    readonly unit: string;
    readonly fraudulentHeaderHash: string;
  };
  readonly scanLabel: string;
  readonly planned: ReturnType<typeof planFaultProofFieldOpening>;
  readonly carriageUtxos: readonly UTxO[];
  readonly fieldReferenceInputs: readonly UTxO[];
  /** 3 = the self-loop back to step-04's own address, 4 = step-05. */
  readonly nextStepIndex: 3 | 4;
  readonly nextStepDatum: string;
  readonly nextScan: SubmitMintAuthorizationStep04Result["nextScan"];
  readonly argsOf: (
    layout: {
      readonly inputIndex: bigint;
      readonly outputIndex: bigint;
    },
    inputsOpening: ReturnType<typeof faultProofFieldOpening>,
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
  const inputsOpening = faultProofFieldOpening({
    planned,
    referenceInputs,
    certificatePolicyId: contracts.fieldPreimageCertificatePolicyId,
    label: `${STEP_LABEL} ${scanLabel}`,
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
      { Continue: [argsOf(layout, inputsOpening)] },
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
    nextScan,
    inputIndex: Number(resolvedLayout.inputIndex),
    outputIndex: Number(resolvedLayout.outputIndex),
    awaitedConfirmation: awaitConfirmation,
  };
};

/** The self-loop: clear the scanned input at the thread's cursor. */
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
    scan,
    scannedItems,
    planned,
    carriageUtxos,
    fieldReferenceInputs,
  } = await prepareStep04(shared);
  const cursor = state.cursor;
  if (cursor < 0n || cursor >= BigInt(scannedItems.length)) {
    throw mintAuthorizationSubmitError(
      `cursor ${cursor.toString()} is outside the committed field ${scan.fieldIndex.toString()}'s ${scannedItems.length.toString()} items — ResolveNext has nothing to resolve.`,
    );
  }
  const outpointKey = Buffer.from(scannedItems[Number(cursor)]);
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
      `the resolved field-${scan.fieldIndex.toString()} input at cursor ${cursor.toString()} carries a reference script hashing to the claimed policy ${state.policy_id} — the absence claim is false.`,
    );
  }
  const ledgerMembershipProof = await buildMintAuthorizationLedgerMembership({
    trie,
    outpointKey,
    priorLedgerRootHex: state.prior_ledger_root,
  });

  const nextState: MintAuthorizationStep04State = {
    ...state,
    cursor: cursor + 1n,
  };
  const nextStepDatum = Data.to(
    { fraud_prover: shared.signer.paymentKeyHash, data: nextState },
    MintAuthorizationStep04Datum,
  );
  return submitPreparedStep04({
    shared,
    threadUtxo,
    threadToken,
    scanLabel: scan.label,
    planned,
    carriageUtxos,
    fieldReferenceInputs,
    nextStepIndex: 3,
    nextStepDatum,
    nextScan: { fieldIndex: nextState.field_index, cursor: nextState.cursor },
    argsOf: (layout, opening) => ({
      ResolveNext: {
        input_index: layout.inputIndex,
        output_index: layout.outputIndex,
        inputs_opening: opening,
        descriptor_cbor: descriptorCborHex,
        ledger_membership_proof: ledgerMembershipProof,
      },
    }),
  });
};

/**
 * Close the scanned field: field 1 moves the scan to field 0 at cursor 0,
 * and field 0 closes it into the direction-A verdict.
 */
export const submitMintAuthorizationStep04AdvanceComplete = async (
  shared: Step04Shared,
): Promise<SubmitMintAuthorizationStep04Result> => {
  const {
    threadUtxo,
    threadToken,
    state,
    scan,
    scannedItems,
    planned,
    carriageUtxos,
    fieldReferenceInputs,
  } = await prepareStep04(shared);
  if (state.cursor !== BigInt(scannedItems.length)) {
    throw mintAuthorizationSubmitError(
      `cursor ${state.cursor.toString()} has not covered the committed field ${scan.fieldIndex.toString()}'s ${scannedItems.length.toString()} items — the scan is incomplete.`,
    );
  }
  const closes = scan.fieldIndex === MIDGARD_FIELD_INDEX.spendInputs;
  const nextState: MintAuthorizationStep04State = {
    ...state,
    field_index: BigInt(MIDGARD_FIELD_INDEX.spendInputs),
    cursor: 0n,
  };
  const step05State: MintAuthorizationStep05State = {
    policy_id: state.policy_id,
    direction: MINT_AUTHORIZATION_DIRECTION_SCRIPT_ABSENT,
  };
  const nextStepDatum = closes
    ? Data.to(
        { fraud_prover: shared.signer.paymentKeyHash, data: step05State },
        MintAuthorizationStep05Datum,
      )
    : Data.to(
        { fraud_prover: shared.signer.paymentKeyHash, data: nextState },
        MintAuthorizationStep04Datum,
      );
  return submitPreparedStep04({
    shared,
    threadUtxo,
    threadToken,
    scanLabel: scan.label,
    planned,
    carriageUtxos,
    fieldReferenceInputs,
    nextStepIndex: closes ? 4 : 3,
    nextStepDatum,
    nextScan: closes
      ? null
      : { fieldIndex: nextState.field_index, cursor: nextState.cursor },
    argsOf: (layout, opening) => ({
      AdvanceComplete: {
        input_index: layout.inputIndex,
        output_index: layout.outputIndex,
        inputs_opening: opening,
      },
    }),
  });
};

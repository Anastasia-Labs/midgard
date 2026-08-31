/**
 * `mint-authorization` step-02 submitter.
 *
 * Binds the thread to the committed claim and reads the accused policy id
 * off the committed field-5 item. The disputed header rides the redeemer,
 * the event's transition step and event→step leaf are opened from the
 * transition-trace reconstruction, and the committed mint field opens
 * through the §8.8 door on whatever §8 carriage tier its own byte length
 * selects — a small mint rides tier-1 inline, a large one is published as
 * tier-2 RawUtxo and read back, never a forced tier. Every check the validator
 * makes that this process can make locally is made locally first, so a
 * doomed transaction is refused before it costs anything:
 *
 * - the reconstruction's header must hash to the thread NFT's asset-name
 *   tail (blake2b-224 of the serialised header Data);
 * - the accused ordinal must land inside the decoded committed mint field
 *   (the decode also re-asserts §5.6 canonicality — non-canonical committed
 *   bytes are the decoding family's dispute, not this one's);
 * - the direction must be in the family's two-value domain.
 *
 * The policy id in the step-03 state is READ off the committed item — the
 * caller names only the ordinal, exactly like the validator.
 */
import { decodeMidgardMintFieldPreimageV1 } from "@al-ft/midgard-core";
import type { MintAuthorizationStep03StateV1 } from "@al-ft/midgard-sdk";
import {
  hashHexWithBlake2b,
  HeaderV1,
  MIDGARD_FIELD_INDEX_V1,
  MINT_AUTHORIZATION_DIRECTION_SCRIPT_ABSENT_V1,
  MINT_AUTHORIZATION_DIRECTION_SCRIPT_UNSATISFIED_V1,
  MintAuthorizationStep02Datum,
  MintAuthorizationStep02SpendRedeemer,
  MintAuthorizationStep03Datum,
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
import { Effect } from "effect";

import {
  faultProofFieldOpeningV1,
  planFaultProofFieldOpeningV1,
  publishFaultProofFieldCarriageV1,
} from "../field-opening-v1.js";
import {
  DEFAULT_CONFIRMATION_POLL_MS,
  type ResolvedProverSigner,
} from "../runtime.js";
import { excludeUtxo } from "../spend-input-witness.js";
import { selectFeeInput } from "../submit-step-01.js";
import type { TransitionTraceReconstruction } from "../transition-trace/reconstruct.js";
import { computationThreadOutputPredicate } from "../tx-layout.js";
import { witnessSpendingValidatorCarriageV1 } from "../witness-reference-scripts-v1.js";
import {
  type FraudProofPreSubmitBoundaryV1,
  reachFraudProofPreSubmitBoundaryV1,
  workflowReferenceScriptsUsedByTransactionV1,
} from "../workflow/transaction-boundary-v1.js";
import type { MintAuthorizationContractsV1 } from "./contracts-v1.js";
import { buildMintAuthorizationStep02EvidenceV1 } from "./evidence-v1.js";
import {
  mintAuthorizationStepLabelV1,
  mintAuthorizationSubmitError,
  requireMintAuthorizationReferenceScriptV1,
  requireMintAuthorizationStepStateV1,
  requireMintAuthorizationThreadUtxoV1,
} from "./submit-common-v1.js";

const STEP_LABEL = mintAuthorizationStepLabelV1(1);

export type SubmitMintAuthorizationStep02Result = {
  readonly txHash: string;
  readonly walletSource: string;
  readonly proverAddress: string;
  readonly fraudProver: string;
  readonly threadOutRef: string;
  readonly nextThreadOutRef: string;
  readonly fraudulentHeaderHash: string;
  readonly computationThreadUnit: string;
  readonly thirdStepAddress: string;
  /** The step-03 state the thread now carries. */
  readonly step03State: MintAuthorizationStep03StateV1;
  readonly inputIndex: number;
  readonly outputIndex: number;
  readonly awaitedConfirmation: boolean;
};

export const submitMintAuthorizationStep02 = async ({
  lucid,
  contracts,
  categoryId,
  signer,
  threadOutRef,
  reconstruction,
  policyIndex,
  direction,
  nativeTxCompactCbor,
  mintItemCbors,
  certificateUtxo,
  referenceScriptUtxo,
  preSubmitBoundary,
  awaitConfirmation = true,
}: {
  readonly lucid: LucidEvolution;
  readonly contracts: MintAuthorizationContractsV1;
  readonly categoryId: string;
  readonly signer: ResolvedProverSigner;
  readonly threadOutRef: string;
  /** The disputed block's transition-trace reconstruction. */
  readonly reconstruction: TransitionTraceReconstruction;
  /** Ordinal of the accused field-5 policy item. */
  readonly policyIndex: bigint;
  /** 0 (script absent) or 1 (script unsatisfied). */
  readonly direction: bigint;
  /** The bound transaction's compact CBOR, hex (the id preimage). */
  readonly nativeTxCompactCbor: string;
  /**
   * The committed field-5 items — each §5.6 mint policy item's canonical
   * bytes, hex, in committed order. The §8 planner re-envelopes them and picks
   * the carriage tier from the resulting preimage's own length; nothing here
   * forces a tier.
   */
  readonly mintItemCbors: readonly string[];
  /** Pre-minted §8.6 certificate when the planner selects tier 3. */
  readonly certificateUtxo?: UTxO;
  /** The mandatory published step-02 reference script. */
  readonly referenceScriptUtxo?: UTxO;
  readonly preSubmitBoundary?: FraudProofPreSubmitBoundaryV1;
  readonly awaitConfirmation?: boolean;
}): Promise<SubmitMintAuthorizationStep02Result> => {
  const { threadUtxo, threadToken } =
    await requireMintAuthorizationThreadUtxoV1({
      lucid,
      contracts,
      categoryId,
      stepIndex: 1,
      threadOutRef,
    });
  const anchorState = requireMintAuthorizationStepStateV1({
    threadUtxo,
    signer,
    schema: MintAuthorizationStep02Datum,
    stepIndex: 1,
  });

  // The header must be the thread NFT's: category id ‖ blake2b-224(header).
  const headerHash = await Effect.runPromise(
    hashHexWithBlake2b(Data.to(reconstruction.header, HeaderV1), 28),
  );
  if (headerHash !== threadToken.fraudulentHeaderHash) {
    throw mintAuthorizationSubmitError(
      `the reconstruction's header hashes to ${headerHash}, not the thread NFT's disputed header ${threadToken.fraudulentHeaderHash}.`,
    );
  }
  if (
    direction !== MINT_AUTHORIZATION_DIRECTION_SCRIPT_ABSENT_V1 &&
    direction !== MINT_AUTHORIZATION_DIRECTION_SCRIPT_UNSATISFIED_V1
  ) {
    throw mintAuthorizationSubmitError(
      `direction ${direction.toString()} is outside {0, 1}.`,
    );
  }
  // The §8.8 door: plan from the committed items (which re-derives and
  // re-commits them against the anchored transaction), then let the planner
  // pick the carriage tier from the resulting preimage's own byte length.
  const planned = planFaultProofFieldOpeningV1({
    fieldIndex: MIDGARD_FIELD_INDEX_V1.mint,
    anchorTxId: anchorState.bad_tx_id,
    nativeTxCompactCbor,
    itemCbors: mintItemCbors.map((hex) => Buffer.from(hex, "hex")),
    owner: signer.paymentKeyHash,
    label: `${STEP_LABEL} mint`,
  });
  const mintItems = decodeMidgardMintFieldPreimageV1(planned.preimage);
  if (policyIndex < 0n || policyIndex >= BigInt(mintItems.length)) {
    throw mintAuthorizationSubmitError(
      `policy index ${policyIndex.toString()} is outside the committed mint field's ${mintItems.length.toString()} items.`,
    );
  }
  const accusedPolicyId = Buffer.from(
    mintItems[Number(policyIndex)].policyId,
  ).toString("hex");

  const evidence = await buildMintAuthorizationStep02EvidenceV1({
    reconstruction,
    eventKey: { L2TransactionEventKey: { tx_id: anchorState.bad_tx_id } },
  });
  const priorLedgerRoot =
    evidence.transitionStepMembership.value.pre_utxos_root;

  const step03State: MintAuthorizationStep03StateV1 = {
    policy_id: accusedPolicyId,
    direction,
    bad_tx_id: anchorState.bad_tx_id,
    bad_tx_witness_set_hash: anchorState.bad_tx_witness_set_hash,
    validity_interval_start: anchorState.validity_interval_start,
    validity_interval_end: anchorState.validity_interval_end,
    prior_ledger_root: priorLedgerRoot,
  };

  signer.selectWallet(lucid);
  // Publish whatever the tier demands (nothing for tier-1 inline), then open
  // against the transaction's COMPLETE reference-input set — the carriage
  // publications, an optional §8.6 certificate, and the step's own reference
  // script all count into the §8.7 positional indices.
  const carriageUtxos = await publishFaultProofFieldCarriageV1({
    lucid,
    signer,
    planned,
    publisherAddress: signer.address,
    label: `${STEP_LABEL} mint`,
  });
  const stepReference =
    referenceScriptUtxo === undefined
      ? undefined
      : requireMintAuthorizationReferenceScriptV1({
          utxo: referenceScriptUtxo,
          expectedScriptHash: contracts.steps[1].spendingScriptHash,
          stepIndex: 1,
        });
  const stepCarriage = witnessSpendingValidatorCarriageV1({
    script: contracts.steps[1].spendingScript,
    referenceUtxo: stepReference,
    label: `${STEP_LABEL} spending validator`,
  });
  const referenceInputs = [
    ...(certificateUtxo === undefined ? [] : [certificateUtxo]),
    ...carriageUtxos,
    ...stepCarriage.referenceInputs,
  ];
  const mintOpening = faultProofFieldOpeningV1({
    planned,
    referenceInputs,
    certificatePolicyId: contracts.fieldPreimageCertificatePolicyId,
    label: `${STEP_LABEL} mint`,
  });
  const walletUtxos = await lucid.wallet().getUtxos();
  const walletUtxosSansCarriage = carriageUtxos.reduce<readonly UTxO[]>(
    (candidates, utxo) => excludeUtxo(candidates, utxo),
    walletUtxos,
  );
  const feeInput = selectFeeInput(walletUtxosSansCarriage);
  const step03Datum = Data.to(
    { fraud_prover: signer.paymentKeyHash, data: step03State },
    MintAuthorizationStep03Datum,
  );
  const step03OutputMatches = computationThreadOutputPredicate({
    address: contracts.steps[2].spendingScriptAddress,
    datum: step03Datum,
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
        step03OutputMatches,
        `${STEP_LABEL} output`,
      ),
    };
    resolvedLayout = layout;
    return Data.to(
      {
        Continue: [
          {
            input_index: layout.inputIndex,
            output_index: layout.outputIndex,
            header: reconstruction.header,
            event_to_step_membership: evidence.eventToStepMembership,
            transition_step_membership: evidence.transitionStepMembership,
            policy_index: policyIndex,
            direction,
            mint_opening: mintOpening,
          },
        ],
      },
      MintAuthorizationStep02SpendRedeemer,
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
      contracts.steps[2].spendingScriptAddress,
      { kind: "inline", value: step03Datum },
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
      "BuildTxWithRedeemer did not resolve the step-02 layout.",
    );
  }
  const signed = await unsigned.sign.withWallet().complete();
  const expectedTxHash = await reachFraudProofPreSubmitBoundaryV1({
    signed,
    referenceScripts: workflowReferenceScriptsUsedByTransactionV1({
      signed,
      candidates: [
        {
          role: "V1 fraud-proof mint-authorization step-02",
          utxo: referenceScriptUtxo,
          expectedScript: contracts.steps[1].spendingScript,
        },
      ],
    }),
    boundary: preSubmitBoundary,
  });
  const txHash = await signed.submit();
  if (txHash !== expectedTxHash) {
    throw mintAuthorizationSubmitError(
      `step-02 provider returned ${txHash}, expected ${expectedTxHash}.`,
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
    threadOutRef,
    nextThreadOutRef: `${txHash}#${resolvedLayout.outputIndex.toString()}`,
    fraudulentHeaderHash: threadToken.fraudulentHeaderHash,
    computationThreadUnit: threadToken.unit,
    thirdStepAddress: contracts.steps[2].spendingScriptAddress,
    step03State,
    inputIndex: Number(resolvedLayout.inputIndex),
    outputIndex: Number(resolvedLayout.outputIndex),
    awaitedConfirmation: awaitConfirmation,
  };
};

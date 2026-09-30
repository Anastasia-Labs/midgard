import {
  decodeMidgardFieldArrayHeader,
  decodeMidgardScriptWitnessFieldPreimage,
  hashMidgardVersionedScript,
} from "@al-ft/midgard-core";
import type {
  MintAuthorizationStep04State,
  MintAuthorizationWitnessScanState,
} from "@al-ft/midgard-sdk";
import {
  MIDGARD_FIELD_INDEX,
  MINT_AUTHORIZATION_DIRECTION_SCRIPT_ABSENT,
  MintAuthorizationStep04Datum,
  MintAuthorizationWitnessScanDatum,
  requireReferenceInputIndex,
} from "@al-ft/midgard-sdk";
import { Data, type UTxO } from "@lucid-evolution/lucid";

import {
  planFaultProofFieldOpening,
  publishFaultProofFieldCarriage,
} from "../field-opening.js";
import { createRawDatumPreimageRequirement } from "../workflow/raw-datum-preimage-prerequisite.js";
import { mintAuthorizationSubmitError } from "./submit-common.js";
import {
  prepareThread,
  STEP_LABEL,
  type Step03Shared,
  type SubmitMintAuthorizationStep03Result,
  submitPreparedStep03,
} from "./submit-mint-authorization-step-03.submit-prepared-step03.js";

/**
 * Direction A's inline half: the committed field 6 holds no script — of any
 * language — hashing to the claimed policy id.
 */
export const submitMintAuthorizationStep03WitnessAbsence = async ({
  scriptTxWitsItemCbors,
  rawPreimageUtxos,
  ...shared
}: Step03Shared & {
  /**
   * The committed field-6 items — each §5.5 versioned-script item's canonical
   * bytes, hex, in committed order. The §8 planner re-envelopes them and picks
   * the carriage tier from the resulting preimage's own byte length.
   */
  readonly scriptTxWitsItemCbors: readonly string[];
  readonly rawPreimageUtxos?: readonly UTxO[];
}): Promise<SubmitMintAuthorizationStep03Result> => {
  const { threadUtxo, threadToken, state } = await prepareThread(shared);
  if (state.direction !== MINT_AUTHORIZATION_DIRECTION_SCRIPT_ABSENT) {
    throw mintAuthorizationSubmitError(
      `the thread's direction is ${state.direction.toString()}; the WitnessAbsence arm is direction A (0).`,
    );
  }
  // Witness field: the door pairs the compact witness set against the thread's
  // anchored `witness_set_hash`, so the plan carries both.
  const planned = planFaultProofFieldOpening({
    anchorSourceKind: 0n,
    fieldIndex: MIDGARD_FIELD_INDEX.scriptWitnesses,
    anchorTxId: state.bad_tx_id,
    nativeTxCompactCbor: shared.nativeTxCompactCbor,
    itemCbors: scriptTxWitsItemCbors.map((hex) => Buffer.from(hex, "hex")),
    owner: shared.signer.paymentKeyHash,
    witnessSet: shared.witnessSet,
    anchorWitnessSetHash: state.bad_tx_witness_set_hash,
    label: `${STEP_LABEL} script-witnesses`,
  });
  // Doomed-transaction refusal: an inline script hashing to the policy
  // makes the absence fold fail on-chain.
  const inlineScripts = decodeMidgardScriptWitnessFieldPreimage(
    planned.preimage,
  );
  for (const script of inlineScripts) {
    if (hashMidgardVersionedScript(script) === state.policy_id) {
      throw mintAuthorizationSubmitError(
        `the committed field 6 carries a script hashing to the claimed policy ${state.policy_id} — the absence claim is false.`,
      );
    }
  }
  shared.signer.selectWallet(shared.lucid);
  const carriageUtxos =
    shared.publishedCarriageUtxos ??
    (await publishFaultProofFieldCarriage({
      lucid: shared.lucid,
      signer: shared.signer,
      planned,
      publisherAddress: shared.signer.address,
      label: `${STEP_LABEL} script-witnesses`,
    }));
  const fieldReferenceInputs = [
    ...(shared.certificateUtxo === undefined ? [] : [shared.certificateUtxo]),
    ...carriageUtxos,
  ];
  if (rawPreimageUtxos !== undefined) {
    const requirement = createRawDatumPreimageRequirement({
      preimage: planned.preimage,
    });
    if (
      rawPreimageUtxos.length !== requirement.publicationDatums.length ||
      rawPreimageUtxos.some(
        (utxo, index) => utxo.datum !== requirement.publicationDatums[index],
      )
    )
      throw mintAuthorizationSubmitError(
        "script witness publications changed ordered preimage",
      );
    const header = decodeMidgardFieldArrayHeader(planned.preimage);
    const initial: MintAuthorizationWitnessScanState = {
      policy_id: state.policy_id,
      bad_tx_id: state.bad_tx_id,
      prior_ledger_root: state.prior_ledger_root,
      field_length: BigInt(planned.preimage.length),
      field_chunk_hashes: [...requirement.publicationDigests],
      cursor: BigInt(header.nextOffset),
      item_index: 0n,
      item_count: BigInt(header.count),
    };
    return submitPreparedStep03({
      shared,
      threadUtxo,
      threadToken,
      planned,
      carriageUtxos: [...carriageUtxos, ...rawPreimageUtxos],
      fieldReferenceInputs: [...fieldReferenceInputs, ...rawPreimageUtxos],
      fieldIndex: MIDGARD_FIELD_INDEX.scriptWitnesses,
      nextStepIndex: 6,
      nextStepDatum: Data.to(
        { fraud_prover: shared.signer.paymentKeyHash, data: initial },
        MintAuthorizationWitnessScanDatum,
      ),
      argsOf: (layout, opening, ctx) => ({
        StartAbsence: {
          input_index: layout.inputIndex,
          output_index: layout.outputIndex,
          chunk_reference_indices: rawPreimageUtxos.map((utxo) =>
            requireReferenceInputIndex(ctx, utxo, "mint witness preimage"),
          ),
          script_tx_wits_opening: opening,
        },
      }),
    });
  }
  const step04State: MintAuthorizationStep04State = {
    policy_id: state.policy_id,
    bad_tx_id: state.bad_tx_id,
    prior_ledger_root: state.prior_ledger_root,
    ref_cursor: 0n,
  };
  const step04Datum = Data.to(
    { fraud_prover: shared.signer.paymentKeyHash, data: step04State },
    MintAuthorizationStep04Datum,
  );
  return submitPreparedStep03({
    shared,
    threadUtxo,
    threadToken,
    planned,
    carriageUtxos,
    fieldReferenceInputs,
    fieldIndex: MIDGARD_FIELD_INDEX.scriptWitnesses,
    nextStepIndex: 3,
    nextStepDatum: step04Datum,
    argsOf: (layout, opening) => ({
      WitnessAbsence: {
        input_index: layout.inputIndex,
        output_index: layout.outputIndex,
        script_tx_wits_opening: opening,
      },
    }),
  });
};

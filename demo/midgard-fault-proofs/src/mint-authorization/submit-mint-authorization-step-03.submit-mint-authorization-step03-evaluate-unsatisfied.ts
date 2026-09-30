import {
  decodeMidgardAddressWitnessFieldPreimage,
  decodeMidgardNativeScript,
  hashMidgardVersionedScript,
  MIDGARD_POSIX_TIME_NONE,
  verifyMidgardNativeScript,
} from "@al-ft/midgard-core";
import type {
  MintAuthorizationEvaluateState,
  MintAuthorizationStep05State,
} from "@al-ft/midgard-sdk";
import {
  hashHexWithBlake2b,
  MIDGARD_FIELD_INDEX,
  MINT_AUTHORIZATION_DIRECTION_SCRIPT_UNSATISFIED,
  MintAuthorizationEvaluateDatum,
  MintAuthorizationStep05Datum,
  requireReferenceInputIndex,
} from "@al-ft/midgard-sdk";
import { Data, type UTxO } from "@lucid-evolution/lucid";
import { Effect } from "effect";

import {
  planFaultProofFieldOpening,
  publishFaultProofFieldCarriage,
} from "../field-opening.js";
import { createRawDatumPreimageRequirement } from "../workflow/raw-datum-preimage-prerequisite.js";
import { mintAuthorizationEvaluationPreimage } from "./evaluate.js";
import { mintAuthorizationSubmitError } from "./submit-common.js";
import {
  prepareThread,
  STEP_LABEL,
  type Step03Shared,
  type SubmitMintAuthorizationStep03Result,
  submitPreparedStep03,
} from "./submit-mint-authorization-step-03.submit-prepared-step03.js";

/**
 * Direction B: the policy's native payload, pinned by hash, evaluates
 * unsatisfied against the committed signer set and validity interval.
 */
export const submitMintAuthorizationStep03EvaluateUnsatisfied = async ({
  scriptBytesHex,
  rawPreimageUtxos,
  addrTxWitsItemCbors,
  ...shared
}: Step03Shared & {
  /** The policy's canonical native payload bytes, hex. */
  readonly scriptBytesHex: string;
  readonly rawPreimageUtxos?: readonly UTxO[];
  /**
   * The committed field-7 items — each §5.3 address-witness item's canonical
   * bytes (fixed 101 bytes), hex, in committed order. The §8 planner
   * re-envelopes them and picks the carriage tier from the resulting
   * preimage's own byte length.
   */
  readonly addrTxWitsItemCbors: readonly string[];
}): Promise<SubmitMintAuthorizationStep03Result> => {
  const { threadUtxo, threadToken, state } = await prepareThread(shared);
  if (state.direction !== MINT_AUTHORIZATION_DIRECTION_SCRIPT_UNSATISFIED) {
    throw mintAuthorizationSubmitError(
      `the thread's direction is ${state.direction.toString()}; the EvaluateUnsatisfied arm is direction B (1).`,
    );
  }
  const decoded = decodeMidgardNativeScript(Buffer.from(scriptBytesHex, "hex"));
  const pinnedHash = hashMidgardVersionedScript({
    language: "NativeCardano",
    scriptBytes: decoded.cbor,
    nativeScript: decoded.script,
  });
  if (pinnedHash !== state.policy_id) {
    throw mintAuthorizationSubmitError(
      `the supplied native payload hashes to ${pinnedHash}, not the claimed policy ${state.policy_id}.`,
    );
  }
  const planned = planFaultProofFieldOpening({
    anchorSourceKind: 0n,
    fieldIndex: MIDGARD_FIELD_INDEX.addressWitnesses,
    anchorTxId: state.bad_tx_id,
    nativeTxCompactCbor: shared.nativeTxCompactCbor,
    itemCbors: addrTxWitsItemCbors.map((hex) => Buffer.from(hex, "hex")),
    owner: shared.signer.paymentKeyHash,
    witnessSet: shared.witnessSet,
    anchorWitnessSetHash: state.bad_tx_witness_set_hash,
    label: `${STEP_LABEL} address-witnesses`,
  });
  const witnesses = decodeMidgardAddressWitnessFieldPreimage(planned.preimage);
  const signerHashes = new Set(
    witnesses.map((witness) =>
      Effect.runSync(
        hashHexWithBlake2b(
          Buffer.from(witness.verificationKey).toString("hex"),
          28,
        ),
      ),
    ),
  );
  const satisfied = verifyMidgardNativeScript(decoded.script, {
    validityIntervalStart:
      state.validity_interval_start === MIDGARD_POSIX_TIME_NONE
        ? undefined
        : state.validity_interval_start,
    validityIntervalEnd:
      state.validity_interval_end === MIDGARD_POSIX_TIME_NONE
        ? undefined
        : state.validity_interval_end,
    witnessSigners: signerHashes,
  });
  if (satisfied) {
    throw mintAuthorizationSubmitError(
      "the committed signer set and validity interval SATISFY the policy's native script — there is no fault to prove.",
    );
  }
  shared.signer.selectWallet(shared.lucid);
  const carriageUtxos =
    shared.publishedCarriageUtxos ??
    (await publishFaultProofFieldCarriage({
      lucid: shared.lucid,
      signer: shared.signer,
      planned,
      publisherAddress: shared.signer.address,
      label: `${STEP_LABEL} address-witnesses`,
    }));
  const fieldReferenceInputs = [
    ...(shared.certificateUtxo === undefined ? [] : [shared.certificateUtxo]),
    ...carriageUtxos,
  ];
  if (rawPreimageUtxos !== undefined) {
    const requirement = createRawDatumPreimageRequirement({
      preimage: mintAuthorizationEvaluationPreimage(
        Buffer.from(scriptBytesHex, "hex"),
        planned.preimage,
      ),
    });
    if (
      rawPreimageUtxos.length !== requirement.publicationDatums.length ||
      rawPreimageUtxos.some(
        (utxo, index) => utxo.datum !== requirement.publicationDatums[index],
      )
    )
      throw mintAuthorizationSubmitError(
        "native payload publications changed ordered preimage",
      );
    const initial: MintAuthorizationEvaluateState = {
      policy_id: state.policy_id,
      script_length: BigInt(scriptBytesHex.length / 2),
      preimage_chunk_hashes: [...requirement.publicationDigests],
      raw_length: BigInt(requirement.preimageHex.length / 2),
      signer_start: BigInt(
        scriptBytesHex.length / 2 +
          planned.preimage.length -
          103 * witnesses.length,
      ),
      signer_count: BigInt(witnesses.length),
      signer_index: 0n,
      signer_hashes: [],
      validity_interval_start: state.validity_interval_start,
      validity_interval_end: state.validity_interval_end,
      cursor: 0n,
      node_count: 0n,
      stack_root: "",
      stack_depth: 0n,
      result: -1n,
    };
    return submitPreparedStep03({
      shared,
      threadUtxo,
      threadToken,
      planned,
      carriageUtxos: [...carriageUtxos, ...rawPreimageUtxos],
      fieldReferenceInputs: [...fieldReferenceInputs, ...rawPreimageUtxos],
      fieldIndex: MIDGARD_FIELD_INDEX.addressWitnesses,
      nextStepIndex: 5,
      nextStepDatum: Data.to(
        { fraud_prover: shared.signer.paymentKeyHash, data: initial },
        MintAuthorizationEvaluateDatum,
      ),
      argsOf: (layout, opening, ctx) => ({
        StartUnsatisfied: {
          script_length: BigInt(scriptBytesHex.length / 2),
          input_index: layout.inputIndex,
          output_index: layout.outputIndex,
          chunk_reference_indices: rawPreimageUtxos.map((utxo) =>
            requireReferenceInputIndex(
              ctx,
              utxo,
              "mint authorization native preimage",
            ),
          ),
          addr_tx_wits_opening: opening,
        },
      }),
    });
  }
  const step05State: MintAuthorizationStep05State = {
    policy_id: state.policy_id,
    direction: MINT_AUTHORIZATION_DIRECTION_SCRIPT_UNSATISFIED,
  };
  const step05Datum = Data.to(
    { fraud_prover: shared.signer.paymentKeyHash, data: step05State },
    MintAuthorizationStep05Datum,
  );
  return submitPreparedStep03({
    shared,
    threadUtxo,
    threadToken,
    planned,
    carriageUtxos,
    fieldReferenceInputs,
    fieldIndex: MIDGARD_FIELD_INDEX.addressWitnesses,
    nextStepIndex: 4,
    nextStepDatum: step05Datum,
    argsOf: (layout, opening) => ({
      EvaluateUnsatisfied: {
        input_index: layout.inputIndex,
        output_index: layout.outputIndex,
        script_bytes: scriptBytesHex,
        addr_tx_wits_opening: opening,
      },
    }),
  });
};

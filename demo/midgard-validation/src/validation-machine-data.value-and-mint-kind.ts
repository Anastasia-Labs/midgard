import { MIDGARD_BOUNDED_ITEM_CHUNK_BYTES } from "@al-ft/midgard-core";
import {
  asArray,
  asBigInt,
  asBytes,
  decodeSingleCbor,
  readCborArrayHeader,
  readCborUnsigned,
} from "@al-ft/midgard-core/codec/cbor";
import { lucidDataFromCborIterative } from "@al-ft/midgard-core/plutus-data-lucid-iterative";

import {
  emptyMidgardInputResolutionSchedule,
  type ValidationMachineWorkWitness,
} from "./validation-machine/index.js";
import { type ValueAndMintStepKind } from "./validation-machine-data.cek-kind.js";

/**
 * `ValueAndMintControlV1` is the twelve-field list the machine writes for
 * every ValueAndMint step; its stage (field 1) and the cursor facts the stage
 * bodies branch on select the semantic resolver, exactly as each on-chain
 * `verify_value_and_mint_<kind>_semantics_v1` pins them (stage, then the
 * remaining replay schedule / replay-asset cursor for stage 2, the output and
 * output-asset cursors for stage 3 and the mint cursor for stage 4). The
 * auxiliary is not consulted: every kind's resolver reconstructs the
 * auxiliary of its own shape from its action fields, so a witness whose
 * auxiliary does not match the kind its control names is refused at the
 * submission encoder, exactly as the resolver would refuse it.
 */
export const valueAndMintKind = (
  witness: ValidationMachineWorkWitness,
): ValueAndMintStepKind => {
  const control = asArray(
    decodeSingleCbor(witness.cbor),
    "value_and_mint_control",
  );
  if (control.length !== 12) {
    throw new Error("value_and_mint_control has an invalid field count");
  }
  const nativeControl = asArray(
    decodeSingleCbor(
      asBytes(control[0], "value_and_mint_control.native_control"),
    ),
    "value_and_mint_control.native_control",
  );
  if (nativeControl.length !== 26) {
    throw new Error(
      "value_and_mint_control native control has an invalid field count",
    );
  }
  const stage = Number(asBigInt(control[1], "value_and_mint_control.stage"));
  if (!Number.isSafeInteger(stage) || stage < 0 || stage > 5) {
    throw new Error("value_and_mint_control stage is invalid");
  }
  switch (stage) {
    case 0:
      return "begin";
    case 1:
      return "replayBegin";
    case 2: {
      const remainingScheduleEmpty = asBytes(
        control[7],
        "value_and_mint_control.replay_remaining_schedule_hash",
      ).equals(emptyMidgardInputResolutionSchedule());
      if (remainingScheduleEmpty) {
        return "replayFinish";
      }
      const replayAssetCursor = asBigInt(
        control[4],
        "value_and_mint_control.replay_asset_cursor",
      );
      return replayAssetCursor === 0n ? "replayInput" : "replayAsset";
    }
    case 3: {
      const outputCursor = asBigInt(
        control[8],
        "value_and_mint_control.output_cursor",
      );
      const outputCount = asBigInt(
        nativeControl[16],
        "value_and_mint_control.native_control.output_count",
      );
      if (outputCursor === outputCount) {
        return "outputFinish";
      }
      const outputAssetCursor = asBigInt(
        control[9],
        "value_and_mint_control.output_asset_cursor",
      );
      return outputAssetCursor === 0n ? "outputDescriptor" : "outputAsset";
    }
    case 4: {
      const mintCursor = asBigInt(
        control[10],
        "value_and_mint_control.mint_cursor",
      );
      const mintCount = asBigInt(
        nativeControl[19],
        "value_and_mint_control.native_control.mint_count",
      );
      return mintCursor === mintCount ? "mintFinish" : "mintAsset";
    }
    default:
      return "finalize";
  }
};

export const nativeScanCursor = (
  witness: ValidationMachineWorkWitness,
): {
  readonly stage: number;
  readonly cursor: number;
  readonly itemLength: number;
} => {
  const control = asArray(
    lucidDataFromCborIterative(witness.cbor),
    "phase_a_native_control",
  );
  if (control.length !== 18) {
    throw new Error("phase-A native control has an invalid field count");
  }
  const exactStage = Number(
    asBigInt(control[5], "phase_a_native_control.stage"),
  );
  const exactCursor = Number(
    asBigInt(control[11], "phase_a_native_control.cursor"),
  );
  const itemLength = Number(
    asBigInt(control[9], "phase_a_native_control.item_length"),
  );
  if (
    !Number.isSafeInteger(itemLength) ||
    itemLength < 0 ||
    exactCursor > itemLength ||
    !Number.isSafeInteger(exactStage) ||
    exactStage < 0 ||
    !Number.isSafeInteger(exactCursor) ||
    exactCursor < 0
  ) {
    throw new Error("phase-A native control stage or cursor is invalid");
  }
  return { stage: exactStage, cursor: exactCursor, itemLength };
};

export const nativePayloadChildCount = ({
  witness,
  cursor,
  itemLength,
  stage,
}: {
  readonly witness: Extract<
    NonNullable<ValidationMachineWorkWitness["auxiliary"]>,
    { readonly kind: "nativeScriptToken" }
  >;
  readonly cursor: number;
  readonly itemLength: number;
  readonly stage: number;
}): number | null => {
  // Aiken returns an empty window here without opening a chunk.
  if (cursor === itemLength) return null;
  const expectedChunkIndex = Math.floor(
    cursor / MIDGARD_BOUNDED_ITEM_CHUNK_BYTES,
  );
  if (witness.chunkProof.chunkIndex !== expectedChunkIndex) {
    throw new Error(
      "phase-A native token proof does not cover the committed cursor",
    );
  }
  const window = Buffer.concat([
    witness.chunkProof.chunk,
    witness.nextChunkProof?.chunk ?? Buffer.alloc(0),
  ]);
  let offset = cursor - expectedChunkIndex * MIDGARD_BOUNDED_ITEM_CHUNK_BYTES;
  try {
    if (stage === 6) {
      offset = readCborUnsigned(
        window,
        offset,
        "phase_a_native_payload.required",
      ).nextOffset;
    }
    return readCborArrayHeader(
      window,
      offset,
      "phase_a_native_payload.children",
    ).length;
  } catch {
    // Both container executors authenticate the window and prove the exact
    // InvalidFieldType successor on malformed payloads. Route to the empty
    // executor; this selection does not accept or repair the payload bytes.
    return null;
  }
};

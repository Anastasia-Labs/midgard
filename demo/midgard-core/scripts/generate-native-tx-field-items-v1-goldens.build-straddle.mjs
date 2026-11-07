import { dirname, join, resolve } from "node:path";
import { fileURLToPath } from "node:url";

import {
  buildMidgardChunkedFieldView,
  decodeMidgardFieldArrayHeader,
  deriveMidgardFieldPreimageCertificate,
  MIDGARD_CHUNK_BYTES_K,
  MIDGARD_MAX_TRANSACTION_AGGREGATE_FIELD_BYTES,
  MIDGARD_MAXIMUM_CARDANO_SPEND_REDEEMER_COUNT,
  midgardFieldItemAt,
  midgardFieldItemExtent,
  midgardFieldStride,
  selectMidgardFieldCarriageTier,
} from "../dist/codec/native-tx-field-access.js";
import {
  encodeMidgardFieldPreimageForField,
  encodeMidgardSpendInputItem,
  midgardFieldCommitmentForField,
} from "../dist/codec/native-tx-field-items.js";
import {
  CARRIAGE_BOUNDARY_LENGTHS,
  midgardV1,
  nativeCardano,
  plutusV3,
  STRADDLE_BLOCK_ITEMS,
  STRADDLE_FIELD_INDEX,
  STRADDLE_ITEM_COUNT,
  STRADDLE_ITEM_INDEX,
  STRADDLE_OWNER,
  STRADDLE_REPEATS,
  STRADDLE_TX_ID,
  straddleInputs,
} from "../tests/fixtures/native-tx-field-items-v1.vectors.mjs";
import {
  goldenChannelEmitter,
  hex,
  parseGoldenChannelArguments,
} from "./golden-channel.mjs";

const scriptDirectory = dirname(fileURLToPath(import.meta.url));

const packageRoot = resolve(scriptDirectory, "..");

export const repositoryRoot = resolve(packageRoot, "../..");

export const generatedJsonPath = join(
  packageRoot,
  "tests/fixtures/native-tx-field-items-v1.generated.json",
);

export const generatedAikenPath = join(
  repositoryRoot,
  "onchain/aiken/lib/midgard/native-tx-field-items-v1-golden.test.ak",
);

const { checkOnly } = parseGoldenChannelArguments(
  "usage: node scripts/generate-native-tx-field-items-v1-goldens.mjs [--check]",
);

export const writeOrCheck = goldenChannelEmitter({ repositoryRoot, checkOnly });

/**
 * §5.3 field 6's three admissible languages, with the Aiken constructor each
 * maps to. Declared once so the JSON entry and the Aiken assertion are built
 * from the same script value rather than from two independent literals.
 */
export const LANGUAGE_TAG_VECTORS = [
  {
    language: "NativeCardano",
    aikenConstructor: "NativeCardanoScript",
    tag: 0,
    script: nativeCardano(90),
  },
  {
    language: "PlutusV3",
    aikenConstructor: "PlutusV3Script",
    tag: 3,
    script: plutusV3(91, 8),
  },
  {
    language: "MidgardV1",
    aikenConstructor: "MidgardV1Script",
    tag: 128,
    script: midgardV1(92, 8),
  },
];

// ---------------------------------------------------------------------------
// §8.4 straddle vector
// ---------------------------------------------------------------------------

/**
 * A real tier-3 field-1 carriage whose item 378 crosses the chunk boundary.
 *
 * The preimage is a 400-byte block of ten distinct stride-40 elements repeated
 * forty times, so both languages rebuild all 16,003 bytes from 400 and hash the
 * chunks for themselves rather than each trusting a digest the other computed.
 * Item `i` therefore carries pattern `i mod 10`, which is what makes an
 * off-by-one read visible: item 377 and item 379 are different bytes from 378.
 *
 * K is 15,148 (§8.3 erratum E1's repaired value) and item 378's payload spans
 * [15,125, 15,163), so reading it stitches 23 bytes out of chunk 0 and 15 out of
 * chunk 1 — the straddle the §8.8 door has to survive, at the stride fields 0/1
 * actually use. The index is a function of K and moved with it; the assertion
 * below is what refuses to emit a vector whose named item does not straddle.
 *
 * **Field 1 rather than field 0**, and the difference is §5.4, not taste: both
 * carry inputs under the same encoder and stride, so the bytes are the same
 * either way, but field 0's cardinality is capped by the Cardano shape bound at
 * 296 spend inputs — 11,843 preimage bytes at that cap, which still selects
 * tier 1. A maximal field 0 cannot reach tier 3, so a field-0 straddle would
 * pin an unreachable configuration. Field 1 has no such bound; §5.4's byte
 * bound alone admits 819 items at stride 40.
 */

/**
 * §5.4/§8.4 reachability, asserted rather than asserted-in-prose: the straddle
 * has to be a configuration the format actually admits *and* one that actually
 * lands above K. Both halves are derived from the published bounds, so moving
 * the vector to a field or a cardinality that cannot reach tier 3 fails the
 * generator instead of quietly pinning a fiction.
 */
const assertStraddleIsReachableV1 = (headerLength, preimageLength) => {
  const stride = midgardFieldStride(STRADDLE_FIELD_INDEX);
  if (STRADDLE_FIELD_INDEX === 0) {
    const maximalSpendInputs =
      headerLength + stride * MIDGARD_MAXIMUM_CARDANO_SPEND_REDEEMER_COUNT;
    throw new Error(
      "§5.4: field 0 is capped at " +
        `${MIDGARD_MAXIMUM_CARDANO_SPEND_REDEEMER_COUNT} spend inputs, so its largest ` +
        `admissible preimage is ${maximalSpendInputs} bytes and can never reach ` +
        `tier 3 (K=${MIDGARD_CHUNK_BYTES_K}); the straddle must live at a field that can`,
    );
  }
  const maximumItems = Math.floor(
    (MIDGARD_MAX_TRANSACTION_AGGREGATE_FIELD_BYTES - headerLength) / stride,
  );
  if (STRADDLE_ITEM_COUNT > maximumItems) {
    throw new Error(
      `§5.4: ${STRADDLE_ITEM_COUNT} items exceed field ${STRADDLE_FIELD_INDEX}'s ` +
        `byte bound of ${maximumItems} at stride ${stride}`,
    );
  }
  if (preimageLength <= MIDGARD_CHUNK_BYTES_K) {
    throw new Error(
      `§8.4: a ${preimageLength}-byte preimage does not exceed ` +
        `K=${MIDGARD_CHUNK_BYTES_K}, so it is not a tier-3 carriage at all`,
    );
  }
};

/**
 * The tier-boundary sweep has to sit *on* `K`, and `K` is a value this module
 * imports while the vector file cannot (see that file's header). Asserted here so
 * a re-pin of `K` — §8.3 erratum E1 moved it once already — cannot leave the
 * sweep pinned at the superseded boundary and go on reporting a partition it is
 * no longer sampling.
 */
export const assertCarriageBoundariesStraddleKV1 = () => {
  for (const required of [MIDGARD_CHUNK_BYTES_K, MIDGARD_CHUNK_BYTES_K + 1]) {
    if (!CARRIAGE_BOUNDARY_LENGTHS.includes(required)) {
      throw new Error(
        `§8.3: CARRIAGE_BOUNDARY_LENGTHS must sample ${required.toString()} ` +
          `(K=${MIDGARD_CHUNK_BYTES_K.toString()} and K+1); got ` +
          `[${CARRIAGE_BOUNDARY_LENGTHS.join(", ")}]`,
      );
    }
  }
};

const straddleItem = (patternIndex) =>
  encodeMidgardSpendInputItem(straddleInputs()[patternIndex]);

export const buildStraddle = () => {
  const selector = {
    fieldIndex: STRADDLE_FIELD_INDEX,
    items: straddleInputs(),
  };
  const preimage = encodeMidgardFieldPreimageForField(selector);
  const commitment = midgardFieldCommitmentForField(selector);
  const header = decodeMidgardFieldArrayHeader(preimage);
  assertStraddleIsReachableV1(header.nextOffset, preimage.length);
  // The repeating block is exactly `STRADDLE_BLOCK_ITEMS` wrapped elements, so
  // the artifacts carry 400 bytes rather than the 16,003 they expand to.
  const block = preimage.subarray(
    header.nextOffset,
    header.nextOffset + STRADDLE_BLOCK_ITEMS * 40,
  );
  const certificate = deriveMidgardFieldPreimageCertificate({
    owner: STRADDLE_OWNER,
    txId: STRADDLE_TX_ID,
    fieldIndex: STRADDLE_FIELD_INDEX,
    preimage,
  });
  const chunks = [];
  for (let start = 0; start < preimage.length; start += MIDGARD_CHUNK_BYTES_K) {
    chunks.push(
      preimage.subarray(
        start,
        Math.min(start + MIDGARD_CHUNK_BYTES_K, preimage.length),
      ),
    );
  }
  const view = buildMidgardChunkedFieldView({
    fieldIndex: STRADDLE_FIELD_INDEX,
    txId: certificate.txId,
    certificate,
    chunks,
    expectedCommitment: commitment,
  });
  const reads = [
    STRADDLE_ITEM_INDEX - 1,
    STRADDLE_ITEM_INDEX,
    STRADDLE_ITEM_INDEX + 1,
  ].map((index) => {
    const extent = midgardFieldItemExtent(view, index);
    return {
      itemIndex: index,
      offset: extent.offset,
      length: extent.length,
      // A read is straddling when its byte range crosses a multiple of K.
      straddles:
        Math.floor(extent.offset / MIDGARD_CHUNK_BYTES_K) !==
        Math.floor((extent.offset + extent.length - 1) / MIDGARD_CHUNK_BYTES_K),
      itemHex: hex(midgardFieldItemAt(view, index)),
    };
  });
  // The whole point of the vector: the middle read, and only the middle read,
  // crosses the boundary. A neighbour that also straddled, or a middle one that
  // did not, would leave the §8.8 stitch untested.
  const straddling = reads.filter((read) => read.straddles);
  if (
    straddling.length !== 1 ||
    straddling[0].itemIndex !== STRADDLE_ITEM_INDEX
  ) {
    throw new Error(
      `§8.4: item ${STRADDLE_ITEM_INDEX} must be the sole straddling read (got ` +
        `${straddling.map((read) => read.itemIndex).join(",") || "none"})`,
    );
  }
  return {
    fieldIndex: STRADDLE_FIELD_INDEX,
    stride: midgardFieldStride(STRADDLE_FIELD_INDEX),
    blockHex: hex(block),
    blockElementCount: STRADDLE_BLOCK_ITEMS,
    repeats: STRADDLE_REPEATS,
    itemCount: STRADDLE_ITEM_COUNT,
    headerHex: hex(preimage.subarray(0, header.nextOffset)),
    totalLength: preimage.length,
    commitmentHex: hex(commitment),
    carriageTier: selectMidgardFieldCarriageTier(preimage.length),
    chunkLengths: chunks.map((chunk) => chunk.length),
    chunkDigestsHex: certificate.chunkDigests.map(hex),
    reads,
    itemsHex: Array.from({ length: STRADDLE_BLOCK_ITEMS }, (_, index) =>
      hex(straddleItem(index)),
    ),
  };
};

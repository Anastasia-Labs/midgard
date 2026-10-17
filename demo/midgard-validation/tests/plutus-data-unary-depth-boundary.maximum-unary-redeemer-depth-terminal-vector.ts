import { readFileSync } from "node:fs";

export const runtimeUnaryDepthWitness = 1_024;

// The exact genuine signed-Cardano unary-depth boundary. The terminal vector
// below carries the same numbers inside one large object comparison; these four
// pins state the cardinality and byte count directly at the search site, so a
// silently shrunk depth can no longer satisfy the relative bounds alone.
export const MAXIMUM_UNARY_DEPTH_ACCEPTED_COUNT = 4_043;

export const MAXIMUM_UNARY_DEPTH_ACCEPTED_SIGNED_BYTES = 16_384;

export const MAXIMUM_UNARY_DEPTH_ADJACENT_COUNT = 4_044;

export const MAXIMUM_UNARY_DEPTH_ADJACENT_SIGNED_BYTES = 16_388;

export const maximumUnaryDepthTerminalVector = {
  maxTxSize: 16_384,
  cardanoSignedCapacityCandidate: {
    acceptedDepth: 4_043,
    acceptedDatumCborBytes: 16_173,
    acceptedSignedCardanoBytes: 16_384,
    signedCardanoByteMargin: 0,
    adjacentDepth: 4_044,
    adjacentDatumCborBytes: 16_177,
    adjacentSignedCardanoBytes: 16_388,
  },
  midgardProjection: {
    dataNodeCount: 4_044,
    traverseSteps: 16_191,
    maximumSourceSpan: 14,
    terminalPreControlCborHex:
      "89010600193f2d193f2d58202ffeeceb1869c66476600e1b0210f55d3f496e48d13b851cab2fea42ad5182ebd87a80d87a80d87a80",
    terminalFrameCborHex:
      "8b000040000040000181820058200349c700d41147fa43955b7c1ee2578d2ef8f08599dd99307121859dd2ee8e860184582087f3ecadbcf7a9f6aacd8fb875358df0898dafbc3e02ac97b94590971260e71201193f29193f2d",
    terminalPostControlCborHex:
      "89010700193f2d193f2d40d87a80d87a80d8799f835820db84befa89735cb7e184bc06890e5b922bcb7e2550caffdff82dcec934fdd723193f2d193f31ff",
    terminalSummary: {
      rootHex:
        "db84befa89735cb7e184bc06890e5b922bcb7e2550caffdff82dcec934fdd723",
      cborLength: "16173",
      memory: "16177",
    },
  },
} as const;

// The exact genuine signed-Cardano *redeemer* unary-depth boundary. Field 8
// carries strictly less unary depth than the inline datum above because the
// spend-redeemer envelope (script witness, redeemer pointer and execution
// units, collateral input/return/total, and the script-data hash) is larger
// than an output's datum option.
export const MAXIMUM_UNARY_REDEEMER_DEPTH_ACCEPTED_COUNT = 3_995;

export const MAXIMUM_UNARY_REDEEMER_DEPTH_ACCEPTED_SIGNED_BYTES = 16_381;

export const MAXIMUM_UNARY_REDEEMER_DEPTH_ADJACENT_COUNT = 3_996;

export const MAXIMUM_UNARY_REDEEMER_DEPTH_ADJACENT_SIGNED_BYTES = 16_385;

export const maximumUnaryRedeemerDepthTerminalVector = {
  maxTxSize: 16384,
  cardanoSignedCapacityCandidate: {
    acceptedDepth: 3995,
    acceptedRedeemerDataCborBytes: 15981,
    acceptedSignedCardanoBytes: 16381,
    signedCardanoByteMargin: 3,
    adjacentDepth: 3996,
    adjacentRedeemerDataCborBytes: 15985,
    adjacentSignedCardanoBytes: 16385,
  },
  midgardProjection: {
    dataNodeCount: 3996,
    traverseSteps: 15999,
    maximumSourceSpan: 14,
    sourceCanonicalTransactionBytes: 16356,
    canonicalTransactionBytes: 16282,
    redeemerFieldBytes: 16000,
    redeemerFieldChunkCount: 4,
    completeFoldStepCount: 8,
    terminalPreControlCborHex:
      "89010600193e6d193e6d582015f635046996af34931eccd00df33c6f1ebe3ec7cb0924403c2c6cc09eb7eba7d87a80d87a80d87a80",
    terminalFrameCborHex:
      "8b000040000040000181820058202e255919cf99b2743582ee389fb462ccb7562cf0a614647a01d9ce9fb14000bc0184582003a6eed9be7ce3bf1e01f104842a3b8fc33bfb24f5941e2fc84019a4bdce9c2001193e69193e6d",
    terminalPostControlCborHex:
      "89010700193e6d193e6d40d87a80d87a80d8799f8358207102b7fc9525a54adf2b32de87dfc544c46f2cf275bd45bfa794dbb518f4fa1e193e6d193e71ff",
    terminalSummary: {
      rootHex:
        "7102b7fc9525a54adf2b32de87dfc544c46f2cf275bd45bfa794dbb518f4fa1e",
      cborLength: "15981",
      memory: "15985",
    },
  },
} as const;

type AlwaysSucceedsBlueprint = {
  readonly validators: readonly {
    readonly title: string;
    readonly compiledCode: string;
  }[];
};

export const alwaysSucceedsCompiledCode = (
  JSON.parse(
    readFileSync(
      new URL(
        "../../midgard-node/blueprints/always-succeeds/plutus.json",
        import.meta.url,
      ),
      "utf8",
    ),
  ) as AlwaysSucceedsBlueprint
).validators.find(
  (validator) => validator.title === "midgard.deposit_spend.else",
)?.compiledCode;

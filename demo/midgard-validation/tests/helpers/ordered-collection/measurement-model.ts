export type MidgardOrderedCollectionBoundaryMeasurement = {
  readonly nativeCanonicalBytes: number;
  readonly fieldBytes: number;
  readonly fieldCommitmentHex: string;
  readonly fieldPreimageCborHex: string;
  readonly fieldPreimageHashHex: string;
  readonly itemCount: number;
  readonly revealStepCount: number;
  readonly completeFoldStepCount: number;
  readonly maxRevealBytes: number;
  readonly maxChunkBytes: number;
  readonly terminalFoldVector: {
    readonly transactionIdHex: string;
    readonly transactionCommitmentHex: string;
    readonly compactCborHex: string;
    readonly witnessSetCompactCborHex: string;
    readonly fieldPreimageLengthsCborHex: string;
    readonly validationContextCborHex: string;
    readonly workWitnessCborHex: string;
    readonly compactBindingWitnessCborHex: string;
    readonly successorPhase: "canonicalDecode" | "compactBinding";
    readonly successorWitnessCborHex: string;
    readonly preWorkRootHex: string;
    readonly postWorkRootHex: string;
    readonly encodedLengthBeforeItem: number;
    readonly collectionProof: {
      readonly fieldIndex: number;
      readonly itemCount: number;
      readonly itemIndex: number;
      readonly itemLength: number;
      readonly itemCommitmentHex: string;
      readonly frontier: readonly {
        readonly height: number;
        readonly hashHex: string;
      }[];
      readonly siblingHexes: readonly string[];
    };
    readonly chunkProof: {
      readonly fieldIndex: number;
      readonly itemIndex: number;
      readonly totalLength: number;
      readonly chunkIndex: number;
      readonly chunkHex: string;
      readonly frontier: readonly {
        readonly height: number;
        readonly hashHex: string;
      }[];
      readonly siblingHexes: readonly string[];
    };
  };
};

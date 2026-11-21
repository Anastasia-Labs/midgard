export type CorpusKind =
  | "valid"
  | "bad-cbor"
  | "bad-signature"
  | "low-fee"
  | "missing-input"
  | "double-spend";
export type JunkKind = Exclude<CorpusKind, "valid">;
export type JunkMix = Record<JunkKind, number>;

export declare const CORPUS_KINDS: readonly CorpusKind[];
export declare const JUNK_KINDS: readonly JunkKind[];

export declare const parseJunkMix: (text: string | undefined) => JunkMix;
export declare const corpusCounts: (
  validCount: number,
  junkMix: Partial<JunkMix>,
) => Record<CorpusKind, number>;

export type CorpusManifest = {
  readonly formatVersion: number;
  readonly seed: string;
  readonly networkId: number;
  readonly minFeeA: string;
  readonly minFeeB: string;
  readonly genesisOutputLovelace: string;
  readonly genesisOutputs: number;
  readonly keyCount: number;
  readonly kinds: readonly CorpusKind[];
  readonly counts: Record<CorpusKind, number>;
  readonly requests: number;
};

export declare const buildCorpus: (options: {
  readonly directory: string;
  readonly seed?: string;
  readonly validCount: number;
  readonly junkMix: Partial<JunkMix>;
  readonly minFeeA?: bigint;
  readonly minFeeB?: bigint;
  readonly keyCount?: number;
  readonly onProgress?: (built: number, total: number) => void;
}) => CorpusManifest;

export type CorpusRequest = {
  readonly kind: CorpusKind;
  readonly body: Buffer;
};

export declare const readCorpus: (directory: string) => {
  readonly manifest: CorpusManifest;
  readonly requests: readonly CorpusRequest[];
};

export declare const readCorpusGenesis: (
  directory: string,
) => Generator<{ readonly outRef: Buffer; readonly output: Buffer }>;

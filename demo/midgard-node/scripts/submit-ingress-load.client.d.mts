export declare const CLIENT_IP_HEADER: "x-midgard-client-ip";
export declare const SUBMIT_CONTENT_TYPE: string;

export declare const syntheticClientIp: (index: number) => string;
export declare const loopbackSourceAddress: (index: number) => string;
export declare const quantile: (
  sorted: readonly number[],
  q: number,
) => number | null;
export declare const outcomeLabel: (
  statusCode: number,
  bodyText: string,
) => string;

export type LatencySummary = {
  readonly count: number;
  readonly p50: number | null;
  readonly p90: number | null;
  readonly p99: number | null;
  readonly max: number | null;
};

export type LoadResult = {
  readonly offeredRatePerSecond: number;
  readonly offeredMs: number;
  readonly wallMs: number;
  readonly sent: number;
  readonly notSentWhenDue: number;
  readonly maxLagRequests: number;
  readonly completed: number;
  readonly admitted: number;
  readonly admittedPerSecond: number;
  readonly outcomes: Record<string, number>;
  readonly byKind: Record<string, Record<string, number>>;
  readonly latencyMs: LatencySummary;
  readonly admittedLatencyMs: LatencySummary;
};

export declare const runLoad: (options: {
  readonly requests: readonly {
    readonly kind: string;
    readonly body: Uint8Array;
  }[];
  readonly target: {
    readonly url: URL;
    readonly socketPath?: string;
    readonly insecure?: boolean;
  };
  readonly ratePerSecond: number;
  readonly durationMs: number;
  readonly maxInFlight?: number;
  readonly keepAliveShare?: number;
  readonly clientIpCount?: number;
  readonly useClientIpHeader?: boolean;
  readonly loopbackSources?: number;
  readonly timeoutMs?: number;
  readonly onProgress?: (progress: {
    readonly elapsedMs: number;
    readonly sent: number;
    readonly inFlight: number;
  }) => void;
}) => Promise<LoadResult>;

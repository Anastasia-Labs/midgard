import {
  createLocalKupmiosHttpOgmiosRawSource,
  type FraudProofRawL1Fetch,
} from "../src/workflow/index.js";
import {
  ANCESTOR,
  EARLIER,
  OgmiosBoundarySocket,
  releaseFinality,
  response,
  TARGET,
  TIP,
} from "./workflow-kupmios-source.ogmios-boundary-socket.js";

export const sourceFixture = ({
  oversizedKupo = false,
  blockTransactions = [],
  kupoMatches = [],
  childAncestor = ANCESTOR,
  parentHeight = 70,
  tipHeight = 100,
  tipSlot = 1000,
  observationDepth,
  checkpointOverride,
  signal,
  timeoutMs,
  maxResponseBytes,
  socketBehavior,
  fetchOverride,
  beforeFetch,
  matchesByPattern,
}: {
  readonly oversizedKupo?: boolean;
  readonly blockTransactions?: readonly unknown[];
  readonly kupoMatches?: readonly unknown[];
  readonly childAncestor?: string;
  readonly parentHeight?: number;
  readonly tipHeight?: number;
  readonly tipSlot?: number;
  readonly observationDepth?: "inclusion" | "release_finality";
  readonly checkpointOverride?: (
    slot: number,
  ) => { slot_no: number; header_hash: string; headHash?: string } | undefined;
  readonly signal?: AbortSignal;
  readonly timeoutMs?: number;
  readonly maxResponseBytes?: number;
  readonly socketBehavior?: ConstructorParameters<
    typeof OgmiosBoundarySocket
  >[3];
  readonly fetchOverride?: FraudProofRawL1Fetch;
  readonly beforeFetch?: (url: string) => Promise<void>;
  readonly matchesByPattern?: (pattern: string) => readonly unknown[];
} = {}) => {
  const requests: {
    readonly url: string;
    readonly init: RequestInit | undefined;
  }[] = [];
  const fetchImpl = async (
    url: string,
    init?: RequestInit,
  ): Promise<Response> => {
    requests.push({ url, init });
    await beforeFetch?.(url);
    if (fetchOverride !== undefined) return await fetchOverride(url, init);
    if (url === "http://127.0.0.1:1337") {
      return response({
        jsonrpc: "2.0",
        id: "midgard-fraud-proof-raw-tip-v1",
        result: { slot: tipSlot, id: TIP, height: tipHeight },
      });
    }
    const checkpointMatch = /\/checkpoints\/(\d+)$/u.exec(url);
    if (checkpointMatch !== null) {
      const slot = Number(checkpointMatch[1]);
      const override = checkpointOverride?.(slot);
      const checkpoint =
        override ??
        (slot >= 400
          ? { slot_no: 400, header_hash: TARGET }
          : slot >= 380
            ? { slot_no: 380, header_hash: ANCESTOR }
            : slot === 379
              ? { slot_no: 360, header_hash: EARLIER }
              : undefined);
      if (checkpoint !== undefined) {
        return response(
          { slot_no: checkpoint.slot_no, header_hash: checkpoint.header_hash },
          true,
          oversizedKupo,
          override?.headHash,
        );
      }
    }
    if (url.includes("/matches/")) {
      const pattern = decodeURIComponent(
        new URL(url).pathname.slice("/matches/".length),
      );
      return response(matchesByPattern?.(pattern) ?? kupoMatches, true);
    }
    throw new Error(`unexpected request ${url}`);
  };
  const sockets: OgmiosBoundarySocket[] = [];
  let resolveSocket!: (socket: OgmiosBoundarySocket) => void;
  const socketCreated = new Promise<OgmiosBoundarySocket>((resolve) => {
    resolveSocket = resolve;
  });
  const source = createLocalKupmiosHttpOgmiosRawSource({
    sourceId: "local-release-test",
    kupoHttpUrl: "http://127.0.0.1:1442",
    ogmiosUrl: "http://127.0.0.1:1337",
    releaseFinality,
    observationDepth,
    fetchImpl,
    ...(signal === undefined ? {} : { signal }),
    ...(timeoutMs === undefined ? {} : { timeoutMs }),
    ...(maxResponseBytes === undefined ? {} : { maxResponseBytes }),
    webSocketFactory: () => {
      const socket = new OgmiosBoundarySocket(
        blockTransactions,
        childAncestor,
        parentHeight,
        { ...socketBehavior, tipHeight, tipSlot },
      );
      sockets.push(socket);
      resolveSocket(socket);
      return socket;
    },
  });
  return { source, requests, sockets, socketCreated };
};

import type { DaAvailabilityReadScope } from "@al-ft/midgard-sdk";
import { readOgmiosBlockTransaction } from "midgard-node/l1-kupmios";
import { canonicalOgmiosBlockDepth } from "midgard-node/services/state-queue-correction-observer.canonical-block-depth";

import type { AcceptanceNativePoint } from "./acceptance-native-boundary.js";
import { createAcceptanceNativeTransport } from "./acceptance-native-transport.js";

const observedFactory =
  (
    factory: ReturnType<
      typeof createAcceptanceNativeTransport
    >["webSocketFactory"],
  ): NonNullable<
    Parameters<typeof readOgmiosBlockTransaction>[0]["webSocketFactory"]
  > =>
  (url) => {
    const websocket = factory(url);
    return {
      send: (text) => websocket.send(text),
      close: (code, reason) => websocket.close(code, reason),
      // The established reader's narrow event type is never; retain its actual
      // event object without decoding or changing the wire frame here.
      addEventListener: (type, listener, options) =>
        websocket.addEventListener(type, listener as EventListener, options),
    };
  };

export const acceptanceNativeLineageReads = (input: {
  endpoint: string;
  scope: DaAvailabilityReadScope;
  point: AcceptanceNativePoint;
  assertCurrent(): void;
  parseJson(text: string): unknown;
}) => {
  const active = new Set<Promise<unknown>>();
  const read = <T>(
    expected: { slot: number; headerHash: string },
    use: (
      factory: ReturnType<
        typeof createAcceptanceNativeTransport
      >["webSocketFactory"],
      timeoutMs: number,
    ) => Promise<T>,
  ): Promise<T> => {
    input.assertCurrent();
    const transport = createAcceptanceNativeTransport({
      ...input,
      validateMessage: (text) => {
        const frame = input.parseJson(text) as {
          method?: unknown;
          result?: { intersection?: unknown; tip?: unknown };
        };
        if (frame.method !== "findIntersection" || frame.result === undefined)
          return;
        const found = frame.result.intersection as {
          slot?: unknown;
          id?: unknown;
        };
        const tip = frame.result.tip as {
          slot?: unknown;
          id?: unknown;
          height?: unknown;
        };
        if (
          found?.slot !== expected.slot ||
          found.id !== expected.headerHash ||
          tip?.id !== input.point.blockHash ||
          String(tip?.slot) !== input.point.slot ||
          String(tip?.height) !== input.point.blockNo
        )
          throw new Error(
            "acceptance lineage selected chain differs from captured native point",
          );
      },
    });
    const timeoutMs = Math.max(1, Math.floor(input.scope.remainingMs()));
    const pending = (async () => {
      let operation: Promise<T> | undefined;
      try {
        operation = use(transport.webSocketFactory, timeoutMs);
        void operation.catch(() => undefined);
        const value = await input.scope.read(() => operation!);
        input.assertCurrent();
        transport.assertCurrent();
        return value;
      } finally {
        await transport.close();
        await operation?.catch(() => undefined);
      }
    })();
    active.add(pending);
    void pending.finally(() => active.delete(pending)).catch(() => undefined);
    return pending;
  };
  return {
    drain: async (): Promise<void> => {
      await Promise.allSettled([...active]);
    },
    readExactTransaction: (
      args: Pick<
        Parameters<typeof readOgmiosBlockTransaction>[0],
        "txHash" | "blockPoint" | "intersection" | "blockScanLimit"
      >,
    ) =>
      read(args.intersection, async (webSocketFactory, timeoutMs) => {
        if (
          !Number.isSafeInteger(args.blockScanLimit) ||
          args.blockScanLimit! < 1 ||
          args.blockScanLimit! > 2161 ||
          !Number.isSafeInteger(args.blockPoint.slot) ||
          args.blockPoint.slot > Number(input.point.slot)
        )
          throw new Error(
            "acceptance native transaction scan exceeds its captured boundary or bounded allowance",
          );
        const value = await readOgmiosBlockTransaction({
          ...args,
          ogmiosUrl: input.endpoint,
          webSocketFactory: observedFactory(webSocketFactory),
          timeoutMs,
        });
        if (BigInt(value.blockPoint.blockNo) > BigInt(input.point.blockNo))
          throw new Error(
            "acceptance transaction block is after the captured native point",
          );
        return value;
      }),
    canonicalBlockDepth: (
      args: Pick<
        Parameters<typeof canonicalOgmiosBlockDepth>[0],
        "blockHash" | "slot" | "blockNo"
      >,
    ) =>
      read(
        { slot: args.slot, headerHash: args.blockHash },
        async (webSocketFactory, timeoutMs) => {
          if (
            args.blockNo < 0n ||
            args.blockNo > BigInt(input.point.blockNo) ||
            !Number.isSafeInteger(args.slot) ||
            args.slot > Number(input.point.slot)
          )
            throw new Error(
              "acceptance canonical block is after the captured native point",
            );
          const depth = await canonicalOgmiosBlockDepth({
            ...args,
            ogmiosUrl: input.endpoint,
            webSocketFactory: observedFactory(webSocketFactory),
            timeoutMs,
          });
          if (
            depth !== null &&
            depth !== BigInt(input.point.blockNo) - args.blockNo + 1n
          )
            throw new Error(
              "acceptance canonical block tip changed from the captured native boundary",
            );
          return depth;
        },
      ),
  };
};

/**
 * The node's one L1-access port (option E, owner 2026-10-10). Every live
 * Lucid client the node, its CLIs and its tools build is built over an
 * adapter's provider, and that provider carries its access: the L1 clock
 * (`slotNow`), the view point reads are answered at, and the submit-slot
 * snapshot. A client built over a provider no adapter opened has no clock,
 * so it cannot take an L1 validity decision: `l1SlotNow` refuses it.
 *
 * Adapters (the kind of process picks one):
 * - `follower`: role processes only (`listen` and its workers). Tracked
 *   reads from the follower store; tip = the follower's covered tip.
 * - `node`: tools, over the local node's ledger alone (no store); tip = the
 *   ledger's chain point.
 * - `kupmios` / `blockfrost`: tools, provider-native; tip = the Ogmios tip
 *   or the latest block.
 *
 * An emulator client keeps answering with its own chain slot.
 */
import type { TransportReadiness } from "@al-ft/l1-node-transport";
import type { SubmitSlotSnapshot } from "@al-ft/midgard-core/ogmios-slot";
import {
  createSlotClock,
  type MonotonicClock,
  type SlotClock,
} from "@al-ft/midgard-l1-follower/heads";
import * as LE from "@lucid-evolution/lucid";
import { Data, Effect } from "effect";

/** No tip has been observed yet, so no L1 decision may be taken: retry. */
export class L1SlotUnknownError extends Data.TaggedError("L1SlotUnknownError")<{
  readonly message: string;
  readonly cause?: unknown;
}> {}

/** A point an adapter's reads are answered at. */
export type L1ViewPoint = Readonly<{ slot: number; id: string }>;

/** Which adapter an access is: the follower one is for roles only. */
export type L1AccessKind = "follower" | "node" | "kupmios" | "blockfrost";

/** The tool adapters a command chooses with `--l1` (`L1_ACCESS`). */
export const TOOL_L1_ACCESS_KINDS = ["node", "kupmios", "blockfrost"] as const;
export type ToolL1AccessKind = (typeof TOOL_L1_ACCESS_KINDS)[number];

/** A tip read is reused for this long before `slotNow` reads again. */
export const L1_TIP_REFRESH_MS = 1_000;

/** What one adapter supplies. */
export type L1AccessAdapter = Readonly<{
  kind: L1AccessKind;
  /** The provider every Lucid over this access reads and submits through. */
  provider: LE.Provider;
  /** The adapter's L1 endpoint, for diagnostics. */
  endpoint: string;
  /** Lucid's slot configuration for the adapter's network. */
  slotConfig: () => Promise<LE.SlotConfig>;
  /** The adapter's tip slot: the clock's only source. */
  tipSlot: () => Promise<number>;
  /** The point reads are answered at now. */
  viewPoint: () => Promise<L1ViewPoint>;
  /**
   * A view point that reflects the chain at least as far as the adapter's
   * tip read first; reads made after it are answered at that point or later.
   */
  synchronizedViewPoint: () => Promise<L1ViewPoint>;
  /** One submit-slot snapshot: bounds a new tx's validity interval. */
  submitSlotSnapshot: () => Promise<SubmitSlotSnapshot>;
  /** The ledger's protocol parameters as CBOR, where the adapter has them. */
  protocolParametersCbor?: () => Promise<Uint8Array>;
  /** The local node transport's readiness, where the adapter has one. */
  transportReadiness?: () => TransportReadiness;
  /** Releases what the adapter opened. */
  close: () => Promise<void>;
}>;

/** What the port adds to an adapter: its clock and its Lucid clients. */
export type L1AccessPort = Readonly<{
  /** `l1SlotNow` for every client over this access (see `slotNow`). */
  slotNow: () => Effect.Effect<number, L1SlotUnknownError>;
  /** Records a tip slot read outside `slotNow`. */
  observeTip: (tipSlot: number) => void;
  /** A Lucid client over this access, on the adapter's slot mapping. */
  lucid: (
    network: LE.Network,
    options?: Omit<LE.LucidOptions, "slotConfig">,
  ) => Promise<LE.LucidEvolution>;
}>;

/** An adapter with its clock: the port the node reads L1 through. */
export type L1Access<A extends L1AccessAdapter = L1AccessAdapter> = A &
  L1AccessPort;

export type L1AccessOptions = Readonly<{
  /** The slot length for the clock; read from `slotConfig` when absent. */
  slotLengthMs?: number;
  monotonicNowMs?: MonotonicClock;
}>;

const L1_ACCESS = Symbol.for("midgard.l1-access");

type Branded = { readonly [L1_ACCESS]?: L1Access };

const monotonicNow: MonotonicClock = () => performance.now();

/**
 * Opens the port over `adapter`: its provider carries the access from now
 * on, so every Lucid built over that provider has the adapter's clock, view
 * and submit slot. One provider belongs to one access.
 */
export const openL1Access = <A extends L1AccessAdapter>(
  adapter: A,
  options: L1AccessOptions = {},
): L1Access<A> => {
  const monotonicNowMs = options.monotonicNowMs ?? monotonicNow;
  let clock: SlotClock | undefined =
    options.slotLengthMs === undefined
      ? undefined
      : createSlotClock({ slotLengthMs: options.slotLengthMs, monotonicNowMs });
  let lastReadAtMs: number | undefined;
  const observeTip = (tipSlot: number): void => {
    if (clock === undefined) return;
    clock.observeTipSlot(tipSlot);
    lastReadAtMs = monotonicNowMs();
  };
  const ensureClock = Effect.gen(function* () {
    if (clock !== undefined) return clock;
    const slotConfig = yield* Effect.tryPromise({
      try: adapter.slotConfig,
      catch: (cause) =>
        new L1SlotUnknownError({
          message: `L1 slot unknown: the ${adapter.kind} access has no slot configuration yet`,
          cause,
        }),
    });
    clock ??= createSlotClock({
      slotLengthMs: slotConfig.slotLength,
      monotonicNowMs,
    });
    return clock;
  });
  // max(tip slot, last tip slot + elapsed / slotLength) on a monotonic clock
  // (plan §3.6). The tip is read again when the last read is older than
  // `L1_TIP_REFRESH_MS`; a failed read keeps the estimate from the last good
  // one. Unknown only while no tip has ever been read.
  const slotNow = (): Effect.Effect<number, L1SlotUnknownError> =>
    Effect.gen(function* () {
      const slotClock = yield* ensureClock;
      let readFailure: unknown;
      if (
        lastReadAtMs === undefined ||
        monotonicNowMs() - lastReadAtMs >= L1_TIP_REFRESH_MS
      ) {
        const read = yield* Effect.either(
          Effect.tryPromise({ try: adapter.tipSlot, catch: (cause) => cause }),
        );
        if (read._tag === "Right") observeTip(read.right);
        else readFailure = read.left;
      }
      const slot = slotClock.slotNow();
      if (slot === null)
        return yield* Effect.fail(
          new L1SlotUnknownError({
            message: `L1 slot unknown: the ${adapter.kind} access has read no tip yet`,
            cause: readFailure,
          }),
        );
      return slot;
    });
  const access: L1Access<A> = {
    ...adapter,
    slotNow,
    observeTip,
    lucid: async (network, lucidOptions = {}) =>
      await LE.Lucid(adapter.provider, network, {
        ...lucidOptions,
        slotConfig: await adapter.slotConfig(),
      }),
  };
  const owner = (adapter.provider as Branded)[L1_ACCESS];
  if (owner !== undefined && owner !== access)
    throw new Error(
      `this provider already belongs to an open ${owner.kind} L1 access`,
    );
  Object.defineProperty(adapter.provider, L1_ACCESS, {
    value: access,
    enumerable: false,
    configurable: true,
  });
  return access;
};

/** The access a provider was opened under, if any. */
export const l1AccessOfProvider = (
  provider: LE.Provider | undefined,
): L1Access | undefined =>
  provider === undefined ? undefined : (provider as Branded)[L1_ACCESS];

/** The access a Lucid client reads L1 through, if it was built over one. */
export const l1AccessOf = (api: LE.LucidEvolution): L1Access | undefined =>
  l1AccessOfProvider(api.config().provider);

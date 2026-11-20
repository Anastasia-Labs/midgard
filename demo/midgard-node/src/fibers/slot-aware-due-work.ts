export type SlotAwareDueWorkKind =
  | "commit_scheduler_refresh"
  | "merge_submit_validity";

export type SlotAwareDueWork = {
  readonly kind: SlotAwareDueWorkKind;
  readonly key: string;
  readonly callerLabel: string;
  readonly reason: string;
  readonly observedSlot: number;
  readonly dueSlot: number;
  readonly dueAtMs: number;
  readonly waitMs: number;
  readonly slotSource: string;
  readonly dependencyKey: string;
  readonly invalidationKey: string;
};

export type SlotAwareDueWorkCheck =
  | {
      readonly status: "missing";
      readonly kind: SlotAwareDueWorkKind;
      readonly key: string;
    }
  | {
      readonly status: "skip";
      readonly entry: SlotAwareDueWork;
      readonly currentSlot: number;
    }
  | {
      readonly status: "due";
      readonly entry: SlotAwareDueWork;
      readonly currentSlot: number;
    }
  | {
      readonly status: "invalidated";
      readonly entry: SlotAwareDueWork;
      readonly currentSlot?: number;
      readonly reason: string;
    };

export type SlotAwareDueWorkRegistry = {
  readonly register: (entry: SlotAwareDueWork) => SlotAwareDueWork;
  readonly clear: (
    kind: SlotAwareDueWorkKind,
    key: string,
  ) => SlotAwareDueWork | undefined;
  readonly peek: (
    kind: SlotAwareDueWorkKind,
    key: string,
  ) => SlotAwareDueWork | undefined;
  readonly check: (input: {
    readonly kind: SlotAwareDueWorkKind;
    readonly key: string;
    readonly currentSlot?: number;
    readonly dependencyKey?: string;
    readonly invalidationKey?: string;
  }) => SlotAwareDueWorkCheck;
  readonly entries: () => readonly SlotAwareDueWork[];
  readonly clearAll: () => void;
};

const registryKey = (kind: SlotAwareDueWorkKind, key: string): string =>
  `${kind}:${key}`;

export const createSlotAwareDueWorkRegistry = (): SlotAwareDueWorkRegistry => {
  const entries = new Map<string, SlotAwareDueWork>();
  return {
    register: (entry) => {
      entries.set(registryKey(entry.kind, entry.key), entry);
      return entry;
    },
    clear: (kind, key) => {
      const mapKey = registryKey(kind, key);
      const entry = entries.get(mapKey);
      entries.delete(mapKey);
      return entry;
    },
    peek: (kind, key) => entries.get(registryKey(kind, key)),
    check: ({ kind, key, currentSlot, dependencyKey, invalidationKey }) => {
      const mapKey = registryKey(kind, key);
      const entry = entries.get(mapKey);
      if (entry === undefined) {
        return { status: "missing", kind, key };
      }
      const invalidationReason =
        dependencyKey !== undefined && dependencyKey !== entry.dependencyKey
          ? "dependency_key_changed"
          : invalidationKey !== undefined &&
              invalidationKey !== entry.invalidationKey
            ? "invalidation_key_changed"
            : undefined;
      if (invalidationReason !== undefined) {
        entries.delete(mapKey);
        return {
          status: "invalidated",
          entry,
          ...(currentSlot === undefined ? {} : { currentSlot }),
          reason: invalidationReason,
        };
      }
      if (currentSlot === undefined) {
        entries.delete(mapKey);
        return {
          status: "invalidated",
          entry,
          reason: "slot_source_unavailable",
        };
      }
      if (currentSlot >= entry.dueSlot) {
        entries.delete(mapKey);
        return { status: "due", entry, currentSlot };
      }
      return { status: "skip", entry, currentSlot };
    },
    entries: () => [...entries.values()],
    clearAll: () => entries.clear(),
  };
};

export const slotAwareDueWorkRegistry = createSlotAwareDueWorkRegistry();

export const registerSlotAwareDueWork = (
  entry: SlotAwareDueWork,
): SlotAwareDueWork => slotAwareDueWorkRegistry.register(entry);

export const clearSlotAwareDueWork = (
  kind: SlotAwareDueWorkKind,
  key: string,
): SlotAwareDueWork | undefined => slotAwareDueWorkRegistry.clear(kind, key);

export const peekSlotAwareDueWork = (
  kind: SlotAwareDueWorkKind,
  key: string,
): SlotAwareDueWork | undefined => slotAwareDueWorkRegistry.peek(kind, key);

export const listSlotAwareDueWork = (): readonly SlotAwareDueWork[] =>
  slotAwareDueWorkRegistry.entries();

export const checkSlotAwareDueWork = (
  input: Parameters<SlotAwareDueWorkRegistry["check"]>[0],
): SlotAwareDueWorkCheck => slotAwareDueWorkRegistry.check(input);

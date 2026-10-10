/**
 * The intent simulation's comparable text: a status, and the whole journal
 * (rows and events), with bigints and bytes as strings.
 */
import {
  type FactStore,
  type Intent,
  type IntentEvent,
  type IntentState,
  readIntentEventsIn,
  readIntentsIn,
} from "../../src/index.js";

export const replacer = (_: string, value: unknown): unknown =>
  typeof value === "bigint"
    ? value.toString()
    : value instanceof Uint8Array
      ? Buffer.from(value).toString("hex")
      : value !== null &&
          typeof value === "object" &&
          (value as { type?: unknown }).type === "Buffer"
        ? Buffer.from((value as { data: number[] }).data).toString("hex")
        : value;

export const statusText = (state: IntentState): string =>
  JSON.stringify(
    { status: state.status, terminal: state.terminalSlot },
    replacer,
  );

export type Journal = Readonly<{ intents: Intent[]; events: IntentEvent[] }>;

export const journalText = (journal: Journal): string[] => [
  ...journal.intents.map((intent) => JSON.stringify(intent, replacer)),
  ...journal.events.map((event) => JSON.stringify(event, replacer)),
];

export const readJournal = (store: FactStore): Promise<Journal> =>
  store.transaction("read", async (tx) => ({
    intents: await readIntentsIn(tx, store.dialect),
    events: await readIntentEventsIn(tx),
  }));

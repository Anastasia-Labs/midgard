import {
  mergePromiseCapacityEvidence,
  parsePromiseCapacityEvidence,
  type PromiseCapacityEvidence,
  promiseCapacityEvidenceKey,
} from "../availability/promise-capacity-evidence.js";
import type { StoreData } from "../store.committee-store.js";
import { parseStoredRecordMap } from "../store.parse-stored-record-map.js";

export const saveJsonPromiseCapacityEvidence = async (
  mutate: (update: (data: StoreData) => StoreData) => Promise<void>,
  record: PromiseCapacityEvidence,
  expectedPointId?: string,
): Promise<PromiseCapacityEvidence> => {
  const canonical = parsePromiseCapacityEvidence(record);
  const key = promiseCapacityEvidenceKey(canonical);
  let saved: PromiseCapacityEvidence | undefined;
  await mutate((data) => {
    if (data.chainCursor?.status === "quarantined")
      throw new Error(
        "Cannot persist capacity evidence while source is quarantined",
      );
    saved = mergePromiseCapacityEvidence(
      data.promiseCapacityEvidence[key],
      canonical,
      expectedPointId,
    );
    return {
      ...data,
      promiseCapacityEvidence: {
        ...data.promiseCapacityEvidence,
        [key]: saved,
      },
    };
  });
  if (saved === undefined)
    throw new Error("Capacity evidence was not persisted");
  return saved;
};

export const parseJsonCapacityMap = (value: unknown) =>
  parseStoredRecordMap(
    value,
    parsePromiseCapacityEvidence,
    promiseCapacityEvidenceKey,
    "promise capacity evidence",
  );
export const jsonCapacityReader =
  (read: () => Promise<StoreData>) =>
  async (key: string): Promise<PromiseCapacityEvidence | undefined> =>
    (await read()).promiseCapacityEvidence[key];
export const jsonCapacityWriter =
  (mutate: (update: (data: StoreData) => StoreData) => Promise<void>) =>
  (record: PromiseCapacityEvidence, expectedPointId?: string) =>
    saveJsonPromiseCapacityEvidence(mutate, record, expectedPointId);

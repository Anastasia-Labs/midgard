import { randomUUID } from "node:crypto";
import { join } from "node:path";
import { DatabaseSync } from "node:sqlite";

import { authorityEnvelopeCodec } from "../../src/runtime/trusted-head-authority.envelope-codec.js";
import {
  initializeSelectedAuthorityStore,
  openWatcherTrustedHeadAuthorityStore,
} from "../../src/runtime/trusted-head-authority.js";
import { authorityRecordCodec } from "../../src/runtime/trusted-head-authority.record-codec.js";
import {
  directory,
  head,
  policy,
  recordAuthenticationKey,
} from "./trusted-head-authority.policy.js";
export const sqliteScene = async (liveRecordLimit = 8) => {
  const input = {
    directory: await directory(),
    policy: policy(),
    recordAuthenticationKey,
    liveRecordLimit,
    generation: `generation-${randomUUID()}`,
  };
  await initializeSelectedAuthorityStore(input);
  const store = await openWatcherTrustedHeadAuthorityStore(input);
  const databasePath = join(
    input.directory,
    input.generation,
    "authority.sqlite",
  );
  const records = authorityRecordCodec(input),
    envelopes = authorityEnvelopeCodec({
      records,
      generation: input.generation,
      liveRecordLimit,
    });
  const mutate = (fn: (db: DatabaseSync) => void) => {
    const db = new DatabaseSync(databasePath);
    try {
      fn(db);
    } finally {
      db.close();
    }
  };
  const advance = async (n: number) => {
    let prior = await store.readCurrent();
    for (let i = 0; i < n; i++) {
      const next = head(
        input.policy,
        prior === null ? 0 : Number(prior.revision) + 1,
        "77",
      );
      await store.compareAndSwap({
        expectedTrustedHead: prior,
        nextTrustedHead: next,
      });
      prior = next;
    }
    return prior;
  };
  return { input, store, databasePath, records, envelopes, mutate, advance };
};

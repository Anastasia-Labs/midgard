import {
  createMemoryLedgerStorage,
  type LedgerStorage,
} from "../src/ledger-store/index.js";
import {
  describeLedgerStoreConformance,
  type OpenedLedgerStorage,
} from "./support/ledger-store-conformance.js";

const openedOver = (storage: LedgerStorage): OpenedLedgerStorage => ({
  storage,
  close: async () => undefined,
  // A new storage object over the same records.
  reopen: async () => openedOver({ ...storage }),
});

describeLedgerStoreConformance("memory", async () =>
  openedOver(createMemoryLedgerStorage()),
);

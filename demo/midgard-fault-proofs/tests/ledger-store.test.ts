import { createMemoryLedgerStorage } from "../src/ledger-store/index.js";
import { describeLedgerStoreConformance } from "./support/ledger-store-conformance.js";

describeLedgerStoreConformance("memory", async () => ({
  storage: createMemoryLedgerStorage(),
  close: async () => undefined,
}));

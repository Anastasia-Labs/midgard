import { fixFitLedgerOwnerEmulator } from "./support/emulator/fit-ledger-owner-emulator.js";
import { registerValueConservationCases } from "./support/value-conservation-lifecycle-cases.js";

fixFitLedgerOwnerEmulator();
registerValueConservationCases("rest");

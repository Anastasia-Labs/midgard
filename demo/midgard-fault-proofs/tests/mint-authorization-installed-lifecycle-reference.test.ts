import { fixFitLedgerOwnerEmulator } from "./support/emulator/fit-ledger-owner-emulator.js";
import { registerMintAuthorizationInstalledCases } from "./support/mint-authorization-installed-lifecycle-cases.js";

fixFitLedgerOwnerEmulator();
registerMintAuthorizationInstalledCases("reference");

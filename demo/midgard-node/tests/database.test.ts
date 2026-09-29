import { registerAdmissionsTests } from "./database.test/admissions.js";
import { registerAvailabilityTests } from "./database.test/availability.js";
import { registerFinalizationTests } from "./database.test/finalization.js";
import { registerDatabaseSetup } from "./database.test/fixtures.js";
import { registerHistoryTests } from "./database.test/history.js";
import { registerInitializationTests } from "./database.test/initialization.js";
import { registerLedgerTests } from "./database.test/ledger.js";
import { registerMpfTests } from "./database.test/mpf.js";
import { registerWithdrawalRecoveryTests } from "./database.test/withdrawal-recovery.js";

registerDatabaseSetup();
registerInitializationTests();
registerAdmissionsTests();
registerAvailabilityTests();
registerFinalizationTests();
registerLedgerTests();
registerHistoryTests();
registerMpfTests();
registerWithdrawalRecoveryTests();

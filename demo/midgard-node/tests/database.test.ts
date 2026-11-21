import { registerAdmissionsTests } from "./database.test/admissions.register-admissions-tests.js";
import { registerAvailabilityTests } from "./database.test/availability.js";
import { registerFinalizationTests } from "./database.test/finalization.register-finalization-tests.js";
import { registerDatabaseSetup } from "./database.test/fixtures.environment.js";
import { registerHistoryTests } from "./database.test/history.js";
import { registerInitializationTests } from "./database.test/initialization.js";
import { registerLedgerTests } from "./database.test/ledger.js";
import { registerMpfTests } from "./database.test/mpf.js";
import { registerPendingInstantCountCase } from "./database.test/pending-instant-counts.js";
import { registerWalletViewTests } from "./database.test/wallet-view.js";
import { registerWithdrawalRecoveryTests } from "./database.test/withdrawal-recovery.js";

registerDatabaseSetup();
registerInitializationTests();
registerAdmissionsTests();
registerAvailabilityTests();
registerFinalizationTests();
registerLedgerTests();
registerHistoryTests();
registerMpfTests();
registerWalletViewTests();
registerWithdrawalRecoveryTests();

registerPendingInstantCountCase();

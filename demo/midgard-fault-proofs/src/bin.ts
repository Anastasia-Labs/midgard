#!/usr/bin/env node

import "./bin.parsed-args.js";
import "node:fs";
import "node:url";
import "@al-ft/midgard-core";
import "@al-ft/midgard-sdk";
import "./evidence/diagnostic-evidence.js";
import "./fabricated-cli-contracts.js";
import "./inspect-contracts.parse-contract-deployment-info.js";
import "./inspect-contracts.inspect-contracts-from-files.js";
import "./json-file.js";
import "./legacy-submission-boundary.js";
import "./prepare-transition-trace.js";
import "./remove-fraudulent-block.submit-remove-fraudulent-block-from-files.js";
import "./remove-unattested-block.submit-unattested-timeout-correction-from-files.js";
import "./submit-da-hash-preimage-step-01.submit-da-hash-preimage-step01-from-files.js";
import "./submit-da-hash-preimage-step-02.js";
import "./submit-fabricated-deposit-step-01.derive-fabricated-deposit-step01-handoff.js";
import "./submit-fabricated-deposit-step-01.submit-fabricated-deposit-step01.js";
import "./submit-fabricated-deposit-step-02.js";
import "./submit-fabricated-deposit-step-03.js";
import "./submit-fabricated-deposit-step-04.js";
import "./submit-fabricated-withdrawal-step-01.derive-fabricated-withdrawal-step01-handoff.js";
import "./submit-fabricated-withdrawal-step-01.submit-fabricated-withdrawal-step01.js";
import "./submit-fabricated-withdrawal-step-02.js";
import "./submit-fabricated-withdrawal-step-03.js";
import "./submit-fabricated-withdrawal-step-04.js";
import "./submit-init.submit-init.js";
import "./submit-input-no-idx-step-01.js";
import "./submit-input-no-idx-step-02.submit-input-no-idx-step02.js";
import "./submit-input-no-idx-step-03.js";
import "./submit-input-no-idx-step-04.submit-input-no-idx-step04-from-files.js";
import "./submit-invalid-range-step-01.submit-invalid-range-step01-from-files.js";
import "./submit-invalid-range-step-02.submit-invalid-range-step02-v1.js";
import "./submit-invalid-signature-step-01.js";
import "./submit-invalid-signature-step-02.submit-invalid-signature-step02-from-files.js";
import "./submit-no-reference-input-step-01.js";
import "./submit-no-reference-input-step-02.js";
import "./submit-no-reference-input-step-03.js";
import "./submit-no-reference-input-step-04.submit-no-reference-input-step04-from-files.js";
import "./submit-reference-input-no-idx-step-01.js";
import "./submit-reference-input-no-idx-step-02.submit-reference-input-no-idx-step02.js";
import "./submit-reference-input-no-idx-step-03.js";
import "./submit-reference-input-no-idx-step-04.submit-reference-input-no-idx-step04-from-files.js";
import "./submit-transition-trace-proof.js";
import "./submit-zero-input-step-01.submit-zero-input-step01-from-files.js";
import "./submit-zero-input-step-02.submit-zero-input-step02-v1.js";
import "./workflow/cli.js";
import "./bin.parse-args.js";
import "./bin.require-validation-one-step-cli-arguments.js";
import "./bin.main.js";

import { isCliEntrypoint } from "./bin.require-validation-one-step-cli-arguments.js";
import { runFaultProofsCli } from "./bin.run-cli.js";

if (
  isCliEntrypoint({ moduleUrl: import.meta.url, argvPath: process.argv[1] })
) {
  void runFaultProofsCli().then((code) => {
    if (code !== 0) process.exitCode = code;
  });
}
export { main } from "./bin.main.js";
export { parseArgs } from "./bin.parse-args.js";
export { type ParsedArgs, parseFraudCategory } from "./bin.parsed-args.js";
export {
  buildRemoveFraudulentBlockCliConfig,
  buildRemoveUnattestedBlockCliConfig,
  isCliEntrypoint,
} from "./bin.require-validation-one-step-cli-arguments.js";

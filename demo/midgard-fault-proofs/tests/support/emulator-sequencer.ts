import { basename } from "node:path";

import { BaseSequencer, type TestSpecification } from "vitest/node";

// These isolated scenarios dominate the emulator lane. Their small entry files
// otherwise sort behind large, faster suites when Vitest has no timing cache.
const expensiveFiles = new Set([
  "submit-init-emulator-transition-trace-final-deep-deposit.test.ts",
  "submit-init-emulator-transition-trace-final-many-assets.test.ts",
]);

export class EmulatorSequencer extends BaseSequencer {
  override async sort(files: TestSpecification[]) {
    const ordered = await super.sort(files);
    return ordered.sort(
      (left, right) =>
        Number(expensiveFiles.has(basename(right.moduleId))) -
        Number(expensiveFiles.has(basename(left.moduleId))),
    );
  }
}

import { basename } from "node:path";

import { BaseSequencer, type TestSpecification } from "vitest/node";

// These isolated scenarios dominate the emulator lane, longest first. Their
// small entry files otherwise sort behind large, faster suites when Vitest has
// no timing cache, and a whale that starts late sets the suite's wall time by
// itself: the value-conservation file alone runs for well over ten minutes.
const expensiveFiles = [
  "value-conservation-lifecycle.test.ts",
  "mint-authorization-installed-lifecycle.test.ts",
  "cek-context-lifecycle.test.ts",
  "submit-init-emulator-transition-trace-final-deep-deposit.test.ts",
  "submit-init-emulator-transition-trace-final-many-assets.test.ts",
];

const rank = (file: TestSpecification): number => {
  const index = expensiveFiles.indexOf(basename(file.moduleId));
  return index === -1 ? expensiveFiles.length : index;
};

export class EmulatorSequencer extends BaseSequencer {
  override async sort(files: TestSpecification[]) {
    const ordered = await super.sort(files);
    return ordered.sort((left, right) => rank(left) - rank(right));
  }
}

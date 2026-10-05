import { vi } from "vitest";

// These coordinator unit fixtures supply unadmitted observations. Model only
// their generation/attestation fence; real native-source suites do not import this.
vi.mock(
  "../../src/l1/local-kupmios-native-observation.js",
  async (loadOriginal) => {
    const actual =
      await loadOriginal<
        typeof import("../../src/l1/local-kupmios-native-observation.js")
      >();
    return {
      ...actual,
      guardWatcherLocalKupmiosNativeObservation: ({
        observation,
        assertCurrent,
      }: Parameters<
        typeof actual.guardWatcherLocalKupmiosNativeObservation
      >[0]) => {
        const guard = () => {
          if (
            "assertCurrent" in observation &&
            typeof observation.assertCurrent === "function"
          )
            observation.assertCurrent();
          assertCurrent();
        };
        guard();
        return Object.freeze({ ...observation, assertCurrent: guard });
      },
    };
  },
);

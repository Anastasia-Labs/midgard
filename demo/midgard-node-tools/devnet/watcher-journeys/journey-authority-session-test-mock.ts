import { vi } from "vitest";

// Session lifecycle tests isolate the reviewed provisioning boundary. Real
// compiled initialization and strict restart are covered in the adjacent suite.
vi.mock("../../src/devnet-stack/watcher-authority-provisioning.js", () => ({
  FRESH_AUTHORITY_PROFILE: { liveRecordLimit: 64 },
  prepareWatcherAuthorityProvisioning: () => ({ allowMissingSecrets: true }),
  finishWatcherAuthorityProvisioning: async () => undefined,
}));

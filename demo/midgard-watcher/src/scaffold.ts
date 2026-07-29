export const WATCHER_PACKAGE_NAME = "midgard-watcher";
export const WATCHER_FOUNDATION_VERSION = 1;
export const WATCHER_NOT_READY_EXIT_CODE = 78;

export type WatcherCommand = "replay" | "start";

export type WatcherFoundationStatus = Readonly<{
  packageName: typeof WATCHER_PACKAGE_NAME;
  scaffoldVersion: typeof WATCHER_FOUNDATION_VERSION;
  state: "foundation_incomplete";
  productionReady: false;
  implementedThrough: "W00";
  requiredBeforeReadiness: readonly ["W01-W46", "WG1", "WG2"];
}>;

export type WatcherCommandIo = Readonly<{
  writeError: (text: string) => void;
}>;

export const WATCHER_FOUNDATION_STATUS: WatcherFoundationStatus = Object.freeze(
  {
    packageName: WATCHER_PACKAGE_NAME,
    scaffoldVersion: WATCHER_FOUNDATION_VERSION,
    state: "foundation_incomplete",
    productionReady: false,
    implementedThrough: "W00",
    requiredBeforeReadiness: ["W01-W46", "WG1", "WG2"] as const,
  },
);

const commandErrorCode = (
  command: WatcherCommand,
): "WATCHER_REPLAY_NOT_READY" | "WATCHER_START_NOT_READY" =>
  command === "replay" ? "WATCHER_REPLAY_NOT_READY" : "WATCHER_START_NOT_READY";

export const runWatcherCommand = (
  command: WatcherCommand,
  io: WatcherCommandIo,
): number => {
  io.writeError(
    `${JSON.stringify({
      ...WATCHER_FOUNDATION_STATUS,
      command,
      errorCode: commandErrorCode(command),
      message:
        "The independent watcher foundation exists, but production operation is disabled until its dependent work packages pass.",
    })}\n`,
  );

  return WATCHER_NOT_READY_EXIT_CODE;
};

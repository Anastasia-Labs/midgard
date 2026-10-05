import { afterEach, describe, expect, it, vi } from "vitest";

import type { DeployContext } from "../src/devnet-stack/deploy.js";
import { walletInfos } from "../src/devnet-stack/identities.js";
import {
  enduranceReasons,
  runEnduranceMaintainer,
} from "../src/devnet-stack/reserve-float-chain.js";
import {
  enduranceReport,
  supervisorMaintainers,
} from "../src/devnet-stack/services.js";

vi.mock(
  "../src/devnet-stack/reserve-float-chain.js",
  async (importOriginal) => ({
    ...(await importOriginal<
      typeof import("../src/devnet-stack/reserve-float-chain.js")
    >()),
    enduranceReasons: vi.fn(),
    runEnduranceMaintainer: vi.fn(),
  }),
);
vi.mock("../src/devnet-stack/identities.js", async (importOriginal) => ({
  ...(await importOriginal<
    typeof import("../src/devnet-stack/identities.js")
  >()),
  walletInfos: vi.fn(),
}));

const wallets = { operator: { address: "addr_operator" } };
const context = {
  layout: { runDir: "/run" },
  run: { network: "devnet" },
  identities: { seeds: {} },
} as unknown as DeployContext;

afterEach(() => vi.resetAllMocks());

describe("the supervisor's maintainers", () => {
  it("run the endurance maintainer on the run's wallets and the stop signal", async () => {
    vi.mocked(walletInfos).mockReturnValue(wallets as never);
    vi.mocked(runEnduranceMaintainer).mockResolvedValue(undefined);
    const maintainers = supervisorMaintainers(context);
    expect(maintainers.map((m) => m.name)).toEqual(["endurance"]);
    const signal = new AbortController().signal;
    await maintainers[0]!.run(signal);
    expect(walletInfos).toHaveBeenCalledWith(context.identities);
    expect(runEnduranceMaintainer).toHaveBeenCalledTimes(1);
    expect(runEnduranceMaintainer).toHaveBeenCalledWith({
      layout: context.layout,
      run: context.run,
      wallets,
      signal,
    });
  });

  it("build the maintainer's inputs inside its run, so a fault there is the maintainer's", async () => {
    vi.mocked(walletInfos).mockImplementation(() => {
      throw new Error("bad seed");
    });
    // Built at once, the fault would escape keepMaintaining's protection.
    const maintainers = supervisorMaintainers(context);
    const signal = new AbortController().signal;
    await expect(
      Promise.resolve().then(() => maintainers[0]!.run(signal)),
    ).rejects.toThrow("bad seed");
    expect(runEnduranceMaintainer).not.toHaveBeenCalled();
  });
});

describe("the status endurance report", () => {
  it("reports the endurance reasons of the run's wallets", async () => {
    vi.mocked(walletInfos).mockReturnValue(wallets as never);
    vi.mocked(enduranceReasons).mockResolvedValue(["reserve_float_low"]);
    expect(await enduranceReport(context)).toEqual(["reserve_float_low"]);
    expect(enduranceReasons).toHaveBeenCalledWith({
      layout: context.layout,
      run: context.run,
      wallets,
    });
  });

  it("reads as its error when the reasons cannot be read, or their inputs cannot be built", async () => {
    vi.mocked(walletInfos).mockReturnValue(wallets as never);
    vi.mocked(enduranceReasons).mockRejectedValue(
      new Error("Kupo answered 503"),
    );
    expect(await enduranceReport(context)).toBe("Error: Kupo answered 503");
    vi.mocked(walletInfos).mockImplementation(() => {
      throw new Error("bad seed");
    });
    expect(await enduranceReport(context)).toBe("Error: bad seed");
  });
});

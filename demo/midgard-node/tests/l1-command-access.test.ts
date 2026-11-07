import { Effect } from "effect";
import { describe, expect, it } from "vitest";

import {
  openToolL1Access,
  selectToolL1Access,
} from "../src/commands/l1-command-access.js";
import { l1AccessOfProvider } from "../src/l1-access.js";
import { openKupmiosAccess } from "../src/l1-external/kupmios-access.js";

const refused = (reason: string, message: RegExp) =>
  expect.objectContaining({
    name: "ToolL1AccessRefusedError",
    reason,
    message: expect.stringMatching(message),
  });

describe("a tool's L1 access selection (--l1 / L1_ACCESS)", () => {
  it("takes the selection, and defaults to node when a local node socket is set", () => {
    expect(selectToolL1Access({ L1_ACCESS: "kupmios" })).toBe("kupmios");
    expect(selectToolL1Access({ L1_ACCESS: " blockfrost " })).toBe(
      "blockfrost",
    );
    expect(
      selectToolL1Access({
        L1_ACCESS: "kupmios",
        L1_NODE_SOCKET_PATH: "/ipc/node.socket",
      }),
    ).toBe("kupmios");
    expect(
      selectToolL1Access({ L1_NODE_SOCKET_PATH: "/ipc/node.socket" }),
    ).toBe("node");
  });

  it("refuses with nothing selected and no local node, naming the options", () => {
    expect(() => selectToolL1Access({})).toThrow(
      refused(
        "tool_l1_access_unselected",
        /--l1 node\|kupmios\|blockfrost \(or L1_ACCESS\).*L1_NODE_SOCKET_PATH/,
      ),
    );
    expect(() =>
      selectToolL1Access({ L1_ACCESS: " ", L1_NODE_SOCKET_PATH: "" }),
    ).toThrow(refused("tool_l1_access_unselected", /No L1 access selected/));
  });

  it("refuses a role's follower access and an unknown access", () => {
    expect(() => selectToolL1Access({ L1_ACCESS: "follower" })).toThrow(
      refused("tool_l1_access_follower", /a role's own access/),
    );
    expect(() => selectToolL1Access({ L1_ACCESS: "kupo" })).toThrow(
      refused("tool_l1_access_unknown", /Unknown L1 access "kupo"/),
    );
  });

  it("refuses an external access missing its settings before opening anything", async () => {
    await expect(
      openToolL1Access({
        network: "Preprod",
        env: { L1_ACCESS: "kupmios", L1_KUPO_URL: "http://kupo" },
      }),
    ).rejects.toThrow(
      refused("tool_l1_access_incomplete", /--l1 kupmios needs L1_OGMIOS_URL/),
    );
    await expect(
      openToolL1Access({
        network: "Preprod",
        env: { L1_ACCESS: "blockfrost" },
      }),
    ).rejects.toThrow(
      refused(
        "tool_l1_access_incomplete",
        /needs L1_BLOCKFROST_URL and L1_BLOCKFROST_PROJECT_ID/,
      ),
    );
    await expect(
      openToolL1Access({
        network: "Custom",
        env: {
          L1_ACCESS: "blockfrost",
          L1_BLOCKFROST_URL: "http://bf",
          L1_BLOCKFROST_PROJECT_ID: "p",
        },
      }),
    ).rejects.toThrow(refused("tool_l1_access_incomplete", /named networks/));
    await expect(
      openToolL1Access({ network: "Preprod", env: { L1_ACCESS: "node" } }),
    ).rejects.toThrow(
      refused("tool_l1_access_incomplete", /--l1 node needs the local node/),
    );
  });

  it("opens the Kupmios adapter, whose provider carries its clock", async () => {
    const access = await openToolL1Access({
      network: "Preprod",
      env: {
        L1_ACCESS: "kupmios",
        L1_KUPO_URL: "http://127.0.0.1:1",
        L1_OGMIOS_URL: "http://127.0.0.1:1",
      },
    });
    try {
      expect(access.kind).toBe("kupmios");
      expect(l1AccessOfProvider(access.provider)).toBe(access);
      expect((await access.slotConfig()).slotLength).toBe(1000);
    } finally {
      await access.close();
    }
    const tipped = openKupmiosAccess({
      network: "Preprod",
      kupoUrl: "http://kupo",
      ogmiosUrl: "http://ogmios",
      fetchImpl: (async () =>
        new Response(
          JSON.stringify({ result: { slot: 500, id: "ab".repeat(32) } }),
        )) as unknown as typeof fetch,
    });
    expect(await Effect.runPromise(tipped.slotNow())).toBeGreaterThanOrEqual(
      500,
    );
  });
});

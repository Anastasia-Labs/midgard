import { describe, expect, it } from "vitest";

import {
  assertRoleL1Env,
  NON_FOLLOWER_L1_ENV_KEYS,
  nonFollowerL1EnvKeys,
  RoleL1AccessRefusedError,
} from "../src/role-l1-env.js";

describe("role L1 env refusal", () => {
  it("accepts a follower-only environment", () => {
    expect(() =>
      assertRoleL1Env(
        {
          L1_NODE_SOCKET_PATH: "/run/cardano/node.socket",
          L1_ACCESS: "follower",
          L1_KUPO_URL: "  ",
        },
        "watcher",
      ),
    ).not.toThrow();
  });

  it("refuses every non-follower L1 setting, naming each", () => {
    for (const key of NON_FOLLOWER_L1_ENV_KEYS)
      expect(nonFollowerL1EnvKeys({ [key]: "x" })).toEqual([key]);
    const error = (() => {
      try {
        assertRoleL1Env(
          { L1_ACCESS: "kupmios", L1_OGMIOS_URL: "http://ogmios:1337" },
          "midgard-node listen",
        );
      } catch (caught) {
        return caught;
      }
      return undefined;
    })();
    expect(error).toBeInstanceOf(RoleL1AccessRefusedError);
    expect(error).toMatchObject({
      reason: "role_non_follower_l1_config",
      keys: ["L1_ACCESS", "L1_OGMIOS_URL"],
    });
    expect((error as Error).message).toContain(
      "midgard-node listen reads L1 only through its follower; refusing to start with a non-follower L1 configuration: L1_ACCESS, L1_OGMIOS_URL",
    );
  });
});

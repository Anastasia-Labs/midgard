package main

import "testing"

func TestRewardAccountOperationBounds(t *testing.T) {
	config := validStartup(t)
	config.Intersection = wirePoint{Kind: "origin"}
	config.Operation = wireOperation{Kind: "reward_account", Credential: &wireStakeCredential{Type: "Script", Hash: repeat("ab", 28)}, TimeoutMs: 10000}
	if err := validateStartup(config); err != nil {
		t.Fatalf("valid ledger query refused: %v", err)
	}
	for name, mutate := range map[string]func(*startupConfig){
		"missing credential": func(c *startupConfig) { c.Operation.Credential = nil },
		"wrong credential type": func(c *startupConfig) {
			c.Operation.Credential = &wireStakeCredential{Type: "Unknown", Hash: repeat("ab", 28)}
		},
		"wrong credential width": func(c *startupConfig) {
			c.Operation.Credential = &wireStakeCredential{Type: "Key", Hash: repeat("ab", 32)}
		},
		"unbounded request":      func(c *startupConfig) { c.Operation.TimeoutMs = 0 },
		"overlong request":       func(c *startupConfig) { c.Operation.TimeoutMs = 120001 },
		"extraneous target":      func(c *startupConfig) { c.Operation.Target = &wireBlockPoint{} },
		"extraneous predecessor": func(c *startupConfig) { c.Operation.PredecessorBlockNo = "0" },
		"credential on stream":   func(c *startupConfig) { c.Operation.Kind = "stream"; c.Operation.TimeoutMs = 0 },
	} {
		t.Run(name, func(t *testing.T) {
			candidate := config
			mutate(&candidate)
			if err := validateStartup(candidate); err == nil {
				t.Fatal("invalid ledger query admitted")
			}
		})
	}
}

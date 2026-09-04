package main

import (
	"encoding/json"
	"net"
	"os"
	"path/filepath"
	"testing"
)

func socketFixture(t *testing.T) string {
	t.Helper()
	directory := t.TempDir()
	path := filepath.Join(directory, "node.socket")
	listener, err := net.Listen("unix", path)
	if err != nil {
		t.Fatalf("listen Unix socket: %v", err)
	}
	t.Cleanup(func() {
		_ = listener.Close()
		_ = os.Remove(path)
	})
	return path
}

func validStartup(t *testing.T) startupConfig {
	t.Helper()
	return startupConfig{
		AuthorityNodeID:       "watcher-node",
		GenesisIdentitySHA256: repeat("11", 32),
		Intersection:          wirePoint{BlockHash: repeat("22", 32), Kind: "point", Slot: "100"},
		Network:               "Preprod",
		NetworkMagic:          1,
		SchemaVersion:         schemaVersion,
		SocketPath:            socketFixture(t),
	}
}

func repeat(value string, count int) string {
	result := ""
	for range count {
		result += value
	}
	return result
}

func TestValidateStartupAuthority(t *testing.T) {
	config := validStartup(t)
	if err := validateStartup(config); err != nil {
		t.Fatalf("valid startup rejected: %v", err)
	}

	wrongMagic := config
	wrongMagic.NetworkMagic = 2
	if err := validateStartup(wrongMagic); err == nil {
		t.Fatal("network-magic substitution admitted")
	}

	implicitOrigin := config
	implicitOrigin.Intersection = wirePoint{}
	if err := validateStartup(implicitOrigin); err == nil {
		t.Fatal("implicit Origin admitted")
	}

	originWithFields := config
	originWithFields.Intersection = wirePoint{BlockHash: repeat("22", 32), Kind: "origin"}
	if err := validateStartup(originWithFields); err == nil {
		t.Fatal("Origin with point fields admitted")
	}

	missingSocket := config
	missingSocket.SocketPath = filepath.Join(t.TempDir(), "missing.socket")
	if err := validateStartup(missingSocket); err == nil {
		t.Fatal("missing native socket admitted")
	}
}

func TestCanonicalWireFieldOrder(t *testing.T) {
	encoded, err := canonicalJSON(rollBackwardEvent{
		Kind:          "roll_backward",
		Point:         wirePoint{BlockHash: repeat("11", 32), Kind: "point", Slot: "10"},
		SchemaVersion: schemaVersion,
		Tip: wireTip{
			BlockHash: repeat("22", 32),
			BlockNo:   "9",
			Kind:      "point",
			Slot:      "11",
		},
	})
	if err != nil {
		t.Fatalf("encode event: %v", err)
	}
	wanted := `{"kind":"roll_backward","point":{"blockHash":"` + repeat("11", 32) + `","kind":"point","slot":"10"},"schemaVersion":"` + schemaVersion + `","tip":{"blockHash":"` + repeat("22", 32) + `","blockNo":"9","kind":"point","slot":"11"}}`
	if string(encoded) != wanted {
		t.Fatalf("non-canonical event\n got: %s\nwant: %s", encoded, wanted)
	}
	var decoded map[string]any
	if err := json.Unmarshal(encoded, &decoded); err != nil {
		t.Fatalf("canonical event is not JSON: %v", err)
	}
}

func TestCanonicalOriginWireShape(t *testing.T) {
	encoded, err := canonicalJSON(rollBackwardEvent{
		Kind:          "roll_backward",
		Point:         wirePoint{Kind: "origin"},
		SchemaVersion: schemaVersion,
		Tip:           wireTip{Kind: "origin"},
	})
	if err != nil {
		t.Fatalf("encode origin: %v", err)
	}
	wanted := `{"kind":"roll_backward","point":{"kind":"origin"},"schemaVersion":"` + schemaVersion + `","tip":{"kind":"origin"}}`
	if string(encoded) != wanted {
		t.Fatalf("non-canonical Origin event\n got: %s\nwant: %s", encoded, wanted)
	}
}

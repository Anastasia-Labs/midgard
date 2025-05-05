package main

import (
	"fmt"
	"os"
	"strings"

	"github.com/Anastasia-Labs/midgard-node-go/cmd"
	"github.com/Anastasia-Labs/midgard-node-go/config"
	"github.com/spf13/cobra"
	"github.com/spf13/viper"
)

var rootCmd = &cobra.Command{
	Use:   "midgard",
	Short: "midgard",
	Long:  `midgard`,
	Run:   func(cmd *cobra.Command, args []string) {},
}

func init() {
	var configValue string
	rootCmd.PersistentFlags().StringVarP(&configValue, "config-file", "c", "./config/config.toml", "config file?")
	configFiles := strings.Split(configValue, ";")
	err := config.ReadConfig(viper.GetViper(), configFiles)
	if err != nil {
		panic("Load config: " + err.Error())
	}
	rootCmd.AddCommand(cmd.NodeCmd)

}

func main() {
	if err := rootCmd.Execute(); err != nil {
		fmt.Fprintf(os.Stderr, "Whoops. There was an error while executing your CLI '%s'", err)
		os.Exit(1)
	}
}

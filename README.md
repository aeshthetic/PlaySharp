# PlaySharp
A terminal youtube client for music that uses youtube-dl and cvlc to download and play music

Building and Running:
```bash
dotnet build
dotnet run --project=src/Downloader -c /path/to/config
```

Publishing:
```bash
dotnet publish -c Release -r linux-x64
```

Usage:
```bash
USAGE: PlaySharp [--help] [--configpath <path>] [--playmode <mode>]
OPTIONS:
    --configpath, -c <path>
                          Specify the path of a config file
    --playmode <mode>     Specify a play mode [autoplay or manual]
    --help                display this list of options.
```

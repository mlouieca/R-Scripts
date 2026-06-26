= (ZipFile as binary) =>
let
    ZipFileBuffer = Binary.Buffer(ZipFile),
    ZipFileSize = Binary.Length(ZipFileBuffer),

    CentralHeaderSignature = 0x02014b50,
    LocalHeaderSignature = 0x04034b50,

    CentralHeaderSize = 42,
    LocalHeaderSize = 30,

    Unsigned16BitLittleEndian =
        BinaryFormat.ByteOrder(BinaryFormat.UnsignedInteger16, ByteOrder.LittleEndian),

    Unsigned32BitLittleEndian =
        BinaryFormat.ByteOrder(BinaryFormat.UnsignedInteger32, ByteOrder.LittleEndian),

    CentralDirectoryHeader = BinaryFormat.Record([
        Version                = Unsigned16BitLittleEndian,
        VersionNeeded          = Unsigned16BitLittleEndian,
        GeneralPurposeFlag     = Unsigned16BitLittleEndian,
        CompressionMethod      = Unsigned16BitLittleEndian,
        LastModifiedTime       = Unsigned16BitLittleEndian,
        LastModifiedDate       = Unsigned16BitLittleEndian,
        CRC32                  = Unsigned32BitLittleEndian,
        CompressedSize         = Unsigned32BitLittleEndian,
        UncompressedSize       = Unsigned32BitLittleEndian,
        FileNameLength         = Unsigned16BitLittleEndian,
        ExtrasLength           = Unsigned16BitLittleEndian,
        FileCommentLength      = Unsigned16BitLittleEndian,
        DiskNumberStarts       = Unsigned16BitLittleEndian,
        InternalFileAttributes = Unsigned16BitLittleEndian,
        ExternalFileAttributes = Unsigned32BitLittleEndian,
        LocalHeaderOffset      = Unsigned32BitLittleEndian
    ]),

    LocalFileHeader = BinaryFormat.Record([
        Signature          = Unsigned32BitLittleEndian,
        VersionNeeded      = Unsigned16BitLittleEndian,
        GeneralPurposeFlag = Unsigned16BitLittleEndian,
        CompressionMethod  = Unsigned16BitLittleEndian,
        LastModifiedTime   = Unsigned16BitLittleEndian,
        LastModifiedDate   = Unsigned16BitLittleEndian,
        CRC32              = Unsigned32BitLittleEndian,
        CompressedSize     = Unsigned32BitLittleEndian,
        UncompressedSize   = Unsigned32BitLittleEndian,
        FileNameLength     = Unsigned16BitLittleEndian,
        ExtrasLength       = Unsigned16BitLittleEndian
    ]),

    EndOfCentralDirectoryRecord = BinaryFormat.Record([
        RestOfFile                      = BinaryFormat.Binary(ZipFileSize - 22),
        EOCDsignature                   = Unsigned32BitLittleEndian,
        NumberOfThisDisk                = Unsigned16BitLittleEndian,
        DiskWhereCentralDirectoryStarts = Unsigned16BitLittleEndian,
        NumberOfRecordsOnThisDisk       = Unsigned16BitLittleEndian,
        TotalNumberOfRecords            = Unsigned16BitLittleEndian,
        CentralDirectorySize            = Unsigned32BitLittleEndian,
        OffsetToStart                   = Unsigned32BitLittleEndian
    ]),

    CentralHeaderFormatter =
        BinaryFormat.Choice(
            Unsigned32BitLittleEndian,
            each
                if _ <> CentralHeaderSignature then
                    BinaryFormat.Record([
                        LocalHeaderOffset = null,
                        CompressedSize = null,
                        UncompressedSize = null,
                        CompressionMethod = null,
                        IsValid = false,
                        Filename = null,
                        CentralDirectoryExtra = null,
                        FileComment = null
                    ])
                else
                    BinaryFormat.Choice(
                        BinaryFormat.Binary(CentralHeaderSize),
                        each
                            let
                                Header = CentralDirectoryHeader(_)
                            in
                                BinaryFormat.Record([
                                    LocalHeaderOffset = Header[LocalHeaderOffset],
                                    CompressedSize = Header[CompressedSize],
                                    UncompressedSize = Header[UncompressedSize],
                                    CompressionMethod = Header[CompressionMethod],
                                    IsValid = true,
                                    Filename = BinaryFormat.Text(Header[FileNameLength]),
                                    CentralDirectoryExtra = BinaryFormat.Binary(Header[ExtrasLength]),
                                    FileComment = BinaryFormat.Binary(Header[FileCommentLength])
                                ]),
                        type binary
                    )
        ),

    EOCDR = EndOfCentralDirectoryRecord(ZipFileBuffer),
    CentralDirectory =
        Binary.Range(
            ZipFileBuffer,
            EOCDR[OffsetToStart],
            EOCDR[CentralDirectorySize]
        ),

    ParsedCentralDirectory =
        BinaryFormat.List(CentralHeaderFormatter, each _[IsValid] = true)(CentralDirectory),

    ValidFiles =
        List.Select(ParsedCentralDirectory, each Record.FieldOrDefault(_, "IsValid", false) = true),

    FilesTable = Table.FromRecords(ValidFiles),

    AddLocalHeader =
        Table.AddColumn(
            FilesTable,
            "LocalHeader",
            each LocalFileHeader(Binary.Range(ZipFileBuffer, [LocalHeaderOffset], LocalHeaderSize))
        ),

    AddDataOffset =
        Table.AddColumn(
            AddLocalHeader,
            "DataOffset",
            each
                [LocalHeaderOffset]
                + LocalHeaderSize
                + [LocalHeader][FileNameLength]
                + [LocalHeader][ExtrasLength],
            Int64.Type
        ),

    AddRawContent =
        Table.AddColumn(
            AddDataOffset,
            "RawContent",
            each Binary.Range(ZipFileBuffer, [DataOffset], [CompressedSize])
        ),

    AddContent =
        Table.AddColumn(
            AddRawContent,
            "Content",
            each
                if [CompressionMethod] = 8 then
                    Binary.Decompress([RawContent], Compression.Deflate)
                else if [CompressionMethod] = 0 then
                    [RawContent]
                else
                    error "Unsupported ZIP compression method: "
                        & Text.From([CompressionMethod])
        ),

    Cleanup =
        Table.RemoveColumns(
            AddContent,
            {"LocalHeader", "RawContent", "CentralDirectoryExtra", "FileComment"}
        )
in
    Cleanup

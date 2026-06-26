= (ZipFile as binary) as table =>
let
    ZipFileBuffer = Binary.Buffer(ZipFile),
    ZipFileSize = Binary.Length(ZipFileBuffer),

    CentralHeaderSignature = 0x02014b50,
    LocalHeaderSignature = 0x04034b50,
    EndOfCentralDirectorySignature = 0x06054b50,
    Zip64EndOfCentralDirectorySignature = 0x06064b50,
    Zip64EndOfCentralDirectoryLocatorSignature = 0x07064b50,
    Zip64ExtraFieldId = 0x0001,

    MaxUInt16 = 65535,
    MaxUInt32 = 4294967295,
    UInt32Factor = 4294967296,
    EOCDMinimumSize = 22,
    EOCDMaximumCommentLength = 65535,
    Zip64LocatorSize = 20,

    Unsigned16BitLittleEndian =
        BinaryFormat.ByteOrder(BinaryFormat.UnsignedInteger16, ByteOrder.LittleEndian),

    Unsigned32BitLittleEndian =
        BinaryFormat.ByteOrder(BinaryFormat.UnsignedInteger32, ByteOrder.LittleEndian),

    ReadUInt16From =
        (Source as binary, Offset as number) as number =>
            Unsigned16BitLittleEndian(Binary.Range(Source, Offset, 2)),

    ReadUInt32From =
        (Source as binary, Offset as number) as number =>
            Unsigned32BitLittleEndian(Binary.Range(Source, Offset, 4)),

    ReadUInt64From =
        (Source as binary, Offset as number) as number =>
            let
                Low = ReadUInt32From(Source, Offset),
                High = ReadUInt32From(Source, Offset + 4)
            in
                High * UInt32Factor + Low,

    ReadTextFrom =
        (Source as binary, Offset as number, Length as number, GeneralPurposeFlag as number) as text =>
            let
                Bytes = Binary.Range(Source, Offset, Length),
                IsUtf8 = Number.BitwiseAnd(GeneralPurposeFlag, 0x0800) <> 0
            in
                if IsUtf8 then
                    Text.FromBinary(Bytes, TextEncoding.Utf8)
                else
                    Text.FromBinary(Bytes),

    Require =
        (Condition as logical, Message as text) as logical =>
            if Condition then true else error Message,

    LastPossibleEOCDOffset = ZipFileSize - EOCDMinimumSize,
    FirstPossibleEOCDOffset =
        List.Max({0, ZipFileSize - EOCDMinimumSize - EOCDMaximumCommentLength}),

    EOCDCandidateOffsets =
        if ZipFileSize < EOCDMinimumSize then
            {}
        else
            List.Numbers(
                LastPossibleEOCDOffset,
                LastPossibleEOCDOffset - FirstPossibleEOCDOffset + 1,
                -1
            ),

    EOCDMatches =
        List.Select(
            EOCDCandidateOffsets,
            each
                ReadUInt32From(ZipFileBuffer, _) = EndOfCentralDirectorySignature
                and _ + EOCDMinimumSize + ReadUInt16From(ZipFileBuffer, _ + 20) = ZipFileSize
        ),

    EOCDOffset =
        if List.IsEmpty(EOCDMatches) then
            error "End of central directory record was not found."
        else
            List.First(EOCDMatches),

    EOCDRecord = [
        NumberOfThisDisk = ReadUInt16From(ZipFileBuffer, EOCDOffset + 4),
        DiskWhereCentralDirectoryStarts = ReadUInt16From(ZipFileBuffer, EOCDOffset + 6),
        NumberOfRecordsOnThisDisk = ReadUInt16From(ZipFileBuffer, EOCDOffset + 8),
        TotalNumberOfRecords = ReadUInt16From(ZipFileBuffer, EOCDOffset + 10),
        CentralDirectorySize = ReadUInt32From(ZipFileBuffer, EOCDOffset + 12),
        OffsetToStart = ReadUInt32From(ZipFileBuffer, EOCDOffset + 16),
        CommentLength = ReadUInt16From(ZipFileBuffer, EOCDOffset + 20)
    ],

    UsesZip64 =
        List.AnyTrue({
            EOCDRecord[NumberOfRecordsOnThisDisk] = MaxUInt16,
            EOCDRecord[TotalNumberOfRecords] = MaxUInt16,
            EOCDRecord[CentralDirectorySize] = MaxUInt32,
            EOCDRecord[OffsetToStart] = MaxUInt32
        }),

    Zip64LocatorOffset = EOCDOffset - Zip64LocatorSize,

    Zip64Locator =
        if UsesZip64 then
            let
                _LocatorExists =
                    Require(
                        Zip64LocatorOffset >= 0
                            and ReadUInt32From(ZipFileBuffer, Zip64LocatorOffset)
                                = Zip64EndOfCentralDirectoryLocatorSignature,
                        "ZIP64 locator is required but was not found before the end of central directory record."
                    )
            in
                if _LocatorExists then
                    [
                        DiskWithZip64EOCD = ReadUInt32From(ZipFileBuffer, Zip64LocatorOffset + 4),
                        Zip64EOCDOffset = ReadUInt64From(ZipFileBuffer, Zip64LocatorOffset + 8),
                        TotalNumberOfDisks = ReadUInt32From(ZipFileBuffer, Zip64LocatorOffset + 16)
                    ]
                else
                    error "ZIP64 locator is required but was not found before the end of central directory record."
        else
            null,

    Zip64EOCDRecord =
        if UsesZip64 then
            let
                Offset = Zip64Locator[Zip64EOCDOffset],
                _SignatureIsValid =
                    Require(
                        ReadUInt32From(ZipFileBuffer, Offset) = Zip64EndOfCentralDirectorySignature,
                        "ZIP64 end of central directory record signature is invalid."
                    )
            in
                if _SignatureIsValid then
                    [
                        SizeOfZip64EOCDRecord = ReadUInt64From(ZipFileBuffer, Offset + 4),
                        VersionMadeBy = ReadUInt16From(ZipFileBuffer, Offset + 12),
                        VersionNeeded = ReadUInt16From(ZipFileBuffer, Offset + 14),
                        NumberOfThisDisk = ReadUInt32From(ZipFileBuffer, Offset + 16),
                        DiskWhereCentralDirectoryStarts = ReadUInt32From(ZipFileBuffer, Offset + 20),
                        NumberOfRecordsOnThisDisk = ReadUInt64From(ZipFileBuffer, Offset + 24),
                        TotalNumberOfRecords = ReadUInt64From(ZipFileBuffer, Offset + 32),
                        CentralDirectorySize = ReadUInt64From(ZipFileBuffer, Offset + 40),
                        OffsetToStart = ReadUInt64From(ZipFileBuffer, Offset + 48)
                    ]
                else
                    error "ZIP64 end of central directory record signature is invalid."
        else
            null,

    _SingleDiskArchive =
        if UsesZip64 then
            Require(
                EOCDRecord[NumberOfThisDisk] = MaxUInt16
                    or EOCDRecord[NumberOfThisDisk] = 0,
                "Multi-disk ZIP archives are not supported."
            )
            and Require(
                EOCDRecord[DiskWhereCentralDirectoryStarts] = MaxUInt16
                    or EOCDRecord[DiskWhereCentralDirectoryStarts] = 0,
                "Multi-disk ZIP archives are not supported."
            )
            and Require(
                Zip64Locator[DiskWithZip64EOCD] = 0
                    and Zip64Locator[TotalNumberOfDisks] = 1
                    and Zip64EOCDRecord[NumberOfThisDisk] = 0
                    and Zip64EOCDRecord[DiskWhereCentralDirectoryStarts] = 0,
                "Multi-disk ZIP64 archives are not supported."
            )
        else
            Require(
                EOCDRecord[NumberOfThisDisk] = 0
                    and EOCDRecord[DiskWhereCentralDirectoryStarts] = 0,
                "Multi-disk ZIP archives are not supported."
            ),

    CentralDirectoryOffset =
        if _SingleDiskArchive and UsesZip64 and EOCDRecord[OffsetToStart] = MaxUInt32 then
            Zip64EOCDRecord[OffsetToStart]
        else
            EOCDRecord[OffsetToStart],

    CentralDirectorySize =
        if UsesZip64 and EOCDRecord[CentralDirectorySize] = MaxUInt32 then
            Zip64EOCDRecord[CentralDirectorySize]
        else
            EOCDRecord[CentralDirectorySize],

    TotalNumberOfRecords =
        if UsesZip64 and EOCDRecord[TotalNumberOfRecords] = MaxUInt16 then
            Zip64EOCDRecord[TotalNumberOfRecords]
        else
            EOCDRecord[TotalNumberOfRecords],

    _CentralDirectoryIsInBounds =
        Require(
            CentralDirectoryOffset >= 0
                and CentralDirectorySize >= 0
                and CentralDirectoryOffset + CentralDirectorySize <= ZipFileSize,
            "Central directory offset or size is outside the ZIP binary."
        ),

    CentralDirectory =
        if _CentralDirectoryIsInBounds then
            Binary.Range(ZipFileBuffer, CentralDirectoryOffset, CentralDirectorySize)
        else
            error "Central directory offset or size is outside the ZIP binary.",

    ParseExtraFields =
        (Extra as binary) as list =>
            let
                ExtraLength = Binary.Length(Extra)
            in
                List.Generate(
                    () => [Offset = 0],
                    each [Offset] + 4 <= ExtraLength,
                    each
                        let
                            FieldSize = ReadUInt16From(Extra, [Offset] + 2),
                            NextOffset = [Offset] + 4 + FieldSize,
                            _FieldIsInBounds =
                                Require(
                                    NextOffset <= ExtraLength,
                                    "ZIP extra field length exceeds available extra-field bytes."
                                )
                        in
                            if _FieldIsInBounds then
                                [Offset = NextOffset]
                            else
                                error "ZIP extra field length exceeds available extra-field bytes.",
                    each
                        let
                            FieldId = ReadUInt16From(Extra, [Offset]),
                            FieldSize = ReadUInt16From(Extra, [Offset] + 2)
                        in
                            [
                                HeaderId = FieldId,
                                Size = FieldSize,
                                Data = Binary.Range(Extra, [Offset] + 4, FieldSize)
                            ]
                ),

    ParseZip64ExtendedInfo =
        (
            Data as binary,
            NeedsUncompressedSize as logical,
            NeedsCompressedSize as logical,
            NeedsLocalHeaderOffset as logical,
            NeedsDiskNumberStarts as logical
        ) as record =>
            let
                Specs =
                    List.Select(
                        {
                            [Name = "UncompressedSize", Bytes = 8, Needed = NeedsUncompressedSize],
                            [Name = "CompressedSize", Bytes = 8, Needed = NeedsCompressedSize],
                            [Name = "LocalHeaderOffset", Bytes = 8, Needed = NeedsLocalHeaderOffset],
                            [Name = "DiskNumberStarts", Bytes = 4, Needed = NeedsDiskNumberStarts]
                        },
                        each [Needed]
                    ),
                Parsed =
                    List.Accumulate(
                        Specs,
                        [Offset = 0, Values = []],
                        (State, Spec) =>
                            let
                                _ValueIsAvailable =
                                    Require(
                                        State[Offset] + Spec[Bytes] <= Binary.Length(Data),
                                        "ZIP64 extended information extra field is missing a required value."
                                    ),
                                Value =
                                    if _ValueIsAvailable and Spec[Bytes] = 8 then
                                        ReadUInt64From(Data, State[Offset])
                                    else if _ValueIsAvailable then
                                        ReadUInt32From(Data, State[Offset])
                                    else
                                        error "ZIP64 extended information extra field is missing a required value.",
                                Values = Record.AddField(State[Values], Spec[Name], Value)
                            in
                                [Offset = State[Offset] + Spec[Bytes], Values = Values]
                    )
            in
                Parsed[Values],

    ParseCentralDirectoryEntry =
        (Offset as number) as record =>
            let
                _CentralHeaderSignatureIsValid =
                    Require(
                        ReadUInt32From(CentralDirectory, Offset) = CentralHeaderSignature,
                        "Invalid central directory header signature."
                    ),
                VersionMadeBy = ReadUInt16From(CentralDirectory, Offset + 4),
                VersionNeeded = ReadUInt16From(CentralDirectory, Offset + 6),
                GeneralPurposeFlag = ReadUInt16From(CentralDirectory, Offset + 8),
                CompressionMethod = ReadUInt16From(CentralDirectory, Offset + 10),
                LastModifiedTime = ReadUInt16From(CentralDirectory, Offset + 12),
                LastModifiedDate = ReadUInt16From(CentralDirectory, Offset + 14),
                CRC32 = ReadUInt32From(CentralDirectory, Offset + 16),
                CompressedSize32 = ReadUInt32From(CentralDirectory, Offset + 20),
                UncompressedSize32 = ReadUInt32From(CentralDirectory, Offset + 24),
                FileNameLength = ReadUInt16From(CentralDirectory, Offset + 28),
                ExtraLength = ReadUInt16From(CentralDirectory, Offset + 30),
                FileCommentLength = ReadUInt16From(CentralDirectory, Offset + 32),
                DiskNumberStarts32 = ReadUInt16From(CentralDirectory, Offset + 34),
                InternalFileAttributes = ReadUInt16From(CentralDirectory, Offset + 36),
                ExternalFileAttributes = ReadUInt32From(CentralDirectory, Offset + 38),
                LocalHeaderOffset32 = ReadUInt32From(CentralDirectory, Offset + 42),
                FileNameOffset = Offset + 46,
                ExtraOffset = FileNameOffset + FileNameLength,
                CommentOffset = ExtraOffset + ExtraLength,
                NextOffset = CommentOffset + FileCommentLength,
                _EntryIsInBounds =
                    Require(
                        NextOffset <= CentralDirectorySize,
                        "Central directory entry extends beyond the central directory."
                    ),
                Filename =
                    ReadTextFrom(
                        CentralDirectory,
                        FileNameOffset,
                        FileNameLength,
                        GeneralPurposeFlag
                    ),
                CentralDirectoryExtra = Binary.Range(CentralDirectory, ExtraOffset, ExtraLength),
                FileComment =
                    ReadTextFrom(
                        CentralDirectory,
                        CommentOffset,
                        FileCommentLength,
                        GeneralPurposeFlag
                    ),
                NeedsUncompressedSize = UncompressedSize32 = MaxUInt32,
                NeedsCompressedSize = CompressedSize32 = MaxUInt32,
                NeedsLocalHeaderOffset = LocalHeaderOffset32 = MaxUInt32,
                NeedsDiskNumberStarts = DiskNumberStarts32 = MaxUInt16,
                NeedsZip64Extra =
                    List.AnyTrue({
                        NeedsUncompressedSize,
                        NeedsCompressedSize,
                        NeedsLocalHeaderOffset,
                        NeedsDiskNumberStarts
                    }),
                ExtraFields = ParseExtraFields(CentralDirectoryExtra),
                Zip64Extra =
                    List.First(
                        List.Select(ExtraFields, each [HeaderId] = Zip64ExtraFieldId),
                        null
                    ),
                _Zip64ExtraExists =
                    Require(
                        not NeedsZip64Extra or Zip64Extra <> null,
                        "Central directory entry requires ZIP64 metadata but does not contain a ZIP64 extra field."
                    ),
                Zip64Values =
                    if Zip64Extra = null then
                        []
                    else
                        ParseZip64ExtendedInfo(
                            Zip64Extra[Data],
                            NeedsUncompressedSize,
                            NeedsCompressedSize,
                            NeedsLocalHeaderOffset,
                            NeedsDiskNumberStarts
                        ),
                ResolvedDiskNumberStarts =
                    if NeedsDiskNumberStarts then
                        Zip64Values[DiskNumberStarts]
                    else
                        DiskNumberStarts32,
                _EntryIsSingleDisk =
                    Require(
                        ResolvedDiskNumberStarts = 0,
                        "Multi-disk ZIP entries are not supported."
                    )
            in
                if
                    _CentralHeaderSignatureIsValid
                        and _EntryIsInBounds
                        and _Zip64ExtraExists
                        and _EntryIsSingleDisk
                then
                    [
                        Filename = Filename,
                        VersionMadeBy = VersionMadeBy,
                        VersionNeeded = VersionNeeded,
                        GeneralPurposeFlag = GeneralPurposeFlag,
                        CompressionMethod = CompressionMethod,
                        LastModifiedTime = LastModifiedTime,
                        LastModifiedDate = LastModifiedDate,
                        CRC32 = CRC32,
                        CompressedSize =
                            if NeedsCompressedSize then
                                Zip64Values[CompressedSize]
                            else
                                CompressedSize32,
                        UncompressedSize =
                            if NeedsUncompressedSize then
                                Zip64Values[UncompressedSize]
                            else
                                UncompressedSize32,
                        LocalHeaderOffset =
                            if NeedsLocalHeaderOffset then
                                Zip64Values[LocalHeaderOffset]
                            else
                                LocalHeaderOffset32,
                        InternalFileAttributes = InternalFileAttributes,
                        ExternalFileAttributes = ExternalFileAttributes,
                        FileComment = FileComment,
                        CentralDirectoryExtra = CentralDirectoryExtra,
                        IsZip64Entry = NeedsZip64Extra,
                        NextOffset = NextOffset
                    ]
                else
                    error "Central directory entry is invalid.",

    CentralDirectoryEntries =
        List.Generate(
            () =>
                if CentralDirectorySize = 0 or TotalNumberOfRecords = 0 then
                    [Done = true, Entry = null, Offset = 0, Index = 0]
                else
                    let
                        Entry = ParseCentralDirectoryEntry(0)
                    in
                        [
                            Done = false,
                            Entry = Entry,
                            Offset = Entry[NextOffset],
                            Index = 1
                        ],
            each not [Done],
            each
                if [Offset] >= CentralDirectorySize or [Index] >= TotalNumberOfRecords then
                    [
                        Done = true,
                        Entry = null,
                        Offset = [Offset],
                        Index = [Index]
                    ]
                else
                    let
                        Entry = ParseCentralDirectoryEntry([Offset])
                    in
                        [
                            Done = false,
                            Entry = Entry,
                            Offset = Entry[NextOffset],
                            Index = [Index] + 1
                        ],
            each [Entry]
        ),

    ParsedDirectoryEnd =
        if List.IsEmpty(CentralDirectoryEntries) then
            0
        else
            List.Last(CentralDirectoryEntries)[NextOffset],

    _CentralDirectoryParsedCleanly =
        Require(
            ParsedDirectoryEnd = CentralDirectorySize
                and List.Count(CentralDirectoryEntries) = TotalNumberOfRecords,
            "Central directory record count or size does not match the parsed entries."
        ),

    FilesTable =
        if _CentralDirectoryParsedCleanly then
            if List.IsEmpty(CentralDirectoryEntries) then
                #table(
                    {
                        "Filename",
                        "VersionMadeBy",
                        "VersionNeeded",
                        "GeneralPurposeFlag",
                        "CompressionMethod",
                        "LastModifiedTime",
                        "LastModifiedDate",
                        "CRC32",
                        "CompressedSize",
                        "UncompressedSize",
                        "LocalHeaderOffset",
                        "InternalFileAttributes",
                        "ExternalFileAttributes",
                        "FileComment",
                        "CentralDirectoryExtra",
                        "IsZip64Entry",
                        "NextOffset"
                    },
                    {}
                )
            else
                Table.FromRecords(CentralDirectoryEntries)
        else
            error "Central directory record count or size does not match the parsed entries.",

    ParseLocalFileHeader =
        (Offset as number) as record =>
            let
                _LocalHeaderIsInBounds =
                    Require(
                        Offset >= 0 and Offset + 30 <= ZipFileSize,
                        "Local file header offset is outside the ZIP binary."
                    ),
                Signature = ReadUInt32From(ZipFileBuffer, Offset),
                _LocalHeaderSignatureIsValid =
                    Require(
                        Signature = LocalHeaderSignature,
                        "Invalid local file header signature."
                    )
            in
                if _LocalHeaderIsInBounds and _LocalHeaderSignatureIsValid then
                    [
                        Signature = Signature,
                        VersionNeeded = ReadUInt16From(ZipFileBuffer, Offset + 4),
                        GeneralPurposeFlag = ReadUInt16From(ZipFileBuffer, Offset + 6),
                        CompressionMethod = ReadUInt16From(ZipFileBuffer, Offset + 8),
                        LastModifiedTime = ReadUInt16From(ZipFileBuffer, Offset + 10),
                        LastModifiedDate = ReadUInt16From(ZipFileBuffer, Offset + 12),
                        CRC32 = ReadUInt32From(ZipFileBuffer, Offset + 14),
                        CompressedSize = ReadUInt32From(ZipFileBuffer, Offset + 18),
                        UncompressedSize = ReadUInt32From(ZipFileBuffer, Offset + 22),
                        FileNameLength = ReadUInt16From(ZipFileBuffer, Offset + 26),
                        ExtrasLength = ReadUInt16From(ZipFileBuffer, Offset + 28)
                    ]
                else
                    error "Invalid local file header.",

    AddLocalHeader =
        Table.AddColumn(
            FilesTable,
            "LocalHeader",
            each ParseLocalFileHeader([LocalHeaderOffset])
        ),

    AddDataOffset =
        Table.AddColumn(
            AddLocalHeader,
            "DataOffset",
            each
                [LocalHeaderOffset]
                    + 30
                    + [LocalHeader][FileNameLength]
                    + [LocalHeader][ExtrasLength],
            Int64.Type
        ),

    ValidateEntry =
        Table.AddColumn(
            AddDataOffset,
            "Validation",
            each
                let
                    IsEncrypted = Number.BitwiseAnd([GeneralPurposeFlag], 1) <> 0,
                    IsSupportedCompression =
                        [CompressionMethod] = 0 or [CompressionMethod] = 8,
                    ContentEnd = [DataOffset] + [CompressedSize]
                in
                    Require(
                        not IsEncrypted,
                        "Encrypted ZIP entries are not supported."
                    )
                    and Require(
                        IsSupportedCompression,
                        "Unsupported ZIP compression method: "
                            & Text.From([CompressionMethod])
                    )
                    and Require(
                        ContentEnd <= ZipFileSize,
                        "ZIP entry compressed content extends beyond the ZIP binary."
                    )
        ),

    AddRawContent =
        Table.AddColumn(
            ValidateEntry,
            "RawContent",
            each
                if [Validation] then
                    Binary.Range(ZipFileBuffer, [DataOffset], [CompressedSize])
                else
                    error "ZIP entry validation failed."
        ),

    AddContent =
        Table.AddColumn(
            AddRawContent,
            "Content",
            each
                if [CompressionMethod] = 8 then
                    Binary.Decompress([RawContent], Compression.Deflate)
                else
                    [RawContent]
        ),

    Cleanup =
        Table.RemoveColumns(
            AddContent,
            {
                "LocalHeader",
                "RawContent",
                "Validation",
                "CentralDirectoryExtra",
                "NextOffset"
            }
        )
in
    Cleanup

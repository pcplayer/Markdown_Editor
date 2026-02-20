unit ObjectConfigSerializer;
{
  将对象的 published 的属性值序列化为 Name=Value 的文本；或者从文本加载到对象；
  用于配置参数的保存和加载；具体的参数对象，从本单元的 TBaseConfig 继承；
  TConfigData 类用于将参数对象的属性值变成 TConfigPropertyInfo 结构体的列表，用于数据传递。
  需要保存和加载参数对象的参数，使用 TObjectConfigSerializer。

  配置使用继承自 TBaseConfig 的类的 publishe 属性，方便代码维护。
  把参数配置对象的 published 属性，输出为通用数据类型的数据，放到 TConfigData 对象里面，
  是方便输出数据给其它地方（比如界面控件）使用。

  2025-10-24
}
interface

uses
  System.SysUtils, System.Classes, System.Rtti, System.TypInfo, Generics.Collections;

type
  ConfigProperty = class(TCustomAttribute)
  private
    FDescription: string;
  public
    constructor Create(const ADescription: string);
    property Description: string read FDescription;
  end;

  // 配置属性信息记录
  TConfigPropertyInfo = record
    Name: string;
    Value: string;
    Description: string;
    DataType: string;
    TypeKind: TTypeKind;
    TypeInfo: PTypeInfo;
    IsValid: Boolean;
    ValidationError: string;
  end;

  //通用数据类型的数据，用于把对象的 published 属性变为通用类型（string, Integer..）存入这里，用来输出给界面控件。
  //因此，配置参数对象如果需要输出数据给界面，可以内部包含一个 TConfigData 类型的对象。
  TConfigData = class
  private
    FProperties: TList<TConfigPropertyInfo>;
    FObjectClass: TClass;
  public
    constructor Create(AObjectClass: TClass);
    destructor Destroy; override;
    function GetPropertyCount: Integer;
    function GetProperty(Index: Integer): TConfigPropertyInfo;
    function FindProperty(const Name: string): TConfigPropertyInfo;
    procedure UpdateProperty(const Name, Value: string);
    property Properties: TList<TConfigPropertyInfo> read FProperties;
    property ObjectClass: TClass read FObjectClass;
  end;

  TBaseConfig = class
  private
    FData: TConfigData; //把属性变成通用的数据类型，用于显示或者其它需要数据的地方。
    FFileName: string;
  public
    constructor Create; virtual;
    destructor Destroy;

    procedure AfterLoad; virtual;
    procedure BeforeSave; virtual;

    property MyData: TConfigData read FData;
  end;

  TObjectConfigSerializer = class
  private
    class function GetPropertyDescription(Obj: TObject; const PropName: string): string;
    class function StringToValue(const ValueStr: string; TypeKind: TTypeKind; TypeInfo: PTypeInfo): TValue;
    class function ValueToString(Value: TValue): string;
    class function GetPropertyTypeName(TypeKind: TTypeKind; TypeInfo: PTypeInfo): string;
  public
    // 文件操作
    class procedure SaveToFile(Obj: TObject; const FileName: string); overload;
    class procedure SaveToFile(Obj: TBaseConfig); overload;

    class procedure LoadFromFile(Obj: TObject; const FileName: string);
    class procedure SaveToStream(Obj: TObject; Stream: TStream);
    class procedure LoadFromStream(Obj: TObject; Stream: TStream);

    // 配置数据操作 - 完全解耦
    class function CreateConfigData(Obj: TObject): TConfigData;
    class procedure ApplyConfigData(ConfigData: TConfigData; Obj: TObject); overload;
    class procedure ApplyConfigData(Obj: TBaseConfig); overload;

    // 验证属性值
    class function ValidatePropertyValue(const ValueStr: string;
      TypeKind: TTypeKind; TypeInfo: PTypeInfo; out ErrorMsg: string): Boolean;

    // 字符串列表操作（用于直接与 TValueListEditor 兼容）
    class procedure ObjectToStrings(Obj: TObject; Strings: TStrings);
    class procedure StringsToObject(Strings: TStrings; Obj: TObject);
  end;

implementation

{ ConfigProperty }

constructor ConfigProperty.Create(const ADescription: string);
begin
  FDescription := ADescription;
end;

{ TConfigData }

constructor TConfigData.Create(AObjectClass: TClass);
begin
  inherited Create;
  FProperties := TList<TConfigPropertyInfo>.Create;
  FObjectClass := AObjectClass;
end;

destructor TConfigData.Destroy;
begin
  FProperties.Free;
  inherited;
end;

function TConfigData.GetPropertyCount: Integer;
begin
  Result := FProperties.Count;
end;

function TConfigData.GetProperty(Index: Integer): TConfigPropertyInfo;
begin
  if (Index >= 0) and (Index < FProperties.Count) then
    Result := FProperties[Index]
  else
    raise EListError.CreateFmt('索引 %d 超出范围', [Index]);
end;

function TConfigData.FindProperty(const Name: string): TConfigPropertyInfo;
var
  I: Integer;
begin
  for I := 0 to FProperties.Count - 1 do
  begin
    if SameText(FProperties[I].Name, Name) then
    begin
      Result := FProperties[I];
      Exit;
    end;
  end;
  // 返回空的记录表示未找到
  FillChar(Result, SizeOf(Result), 0);
end;

procedure TConfigData.UpdateProperty(const Name, Value: string);
var
  I: Integer;
  PropInfo: TConfigPropertyInfo;
  ErrorMsg: string;
begin
  for I := 0 to FProperties.Count - 1 do
  begin
    if SameText(FProperties[I].Name, Name) then
    begin
      PropInfo := FProperties[I];
      PropInfo.Value := Value;

      // 验证新值
      PropInfo.IsValid := TObjectConfigSerializer.ValidatePropertyValue(
        Value, PropInfo.TypeKind, PropInfo.TypeInfo, ErrorMsg);
      PropInfo.ValidationError := ErrorMsg;

      FProperties[I] := PropInfo;
      Exit;
    end;
  end;
end;

{ TBaseConfig }

procedure TBaseConfig.AfterLoad;
begin
  // 子类可以重写
end;

procedure TBaseConfig.BeforeSave;
begin
  // 子类可以重写
end;

constructor TBaseConfig.Create;
begin
  FData := TObjectConfigSerializer.CreateConfigData(Self);
end;

destructor TBaseConfig.Destroy;
begin
  FData.Free;
end;

{ TObjectConfigSerializer }

class function TObjectConfigSerializer.GetPropertyDescription(Obj: TObject; const PropName: string): string;
var
  Context: TRttiContext;
  AType: TRttiType;
  Prop: TRttiProperty;
  Attr: TCustomAttribute;
begin
  Result := '';
  Context := TRttiContext.Create;
  try
    AType := Context.GetType(Obj.ClassType);
    Prop := AType.GetProperty(PropName);
    if Assigned(Prop) then
    begin
      for Attr in Prop.GetAttributes do
      begin
        if Attr is ConfigProperty then
        begin
          Result := ConfigProperty(Attr).Description;
          Break;
        end;
      end;
    end;
  finally
    Context.Free;
  end;
end;

class function TObjectConfigSerializer.GetPropertyTypeName(TypeKind: TTypeKind; TypeInfo: PTypeInfo): string;
begin
  case TypeKind of
    tkInteger, tkInt64:
      Result := '整数';
    tkFloat:
      begin
        if TypeInfo = System.TypeInfo(TDateTime) then
          Result := '日期时间'
        else if TypeInfo = System.TypeInfo(TDate) then
          Result := '日期'
        else if TypeInfo = System.TypeInfo(TTime) then
          Result := '时间'
        else
          Result := '浮点数';
      end;
    tkEnumeration:
      begin
        if TypeInfo = System.TypeInfo(Boolean) then
          Result := '布尔值'
        else
          Result := '枚举';
      end;
    tkString, tkLString, tkWString, tkUString:
      Result := '字符串';
    tkSet:
      Result := '集合';
  else
    Result := '未知类型';
  end;
end;

class function TObjectConfigSerializer.StringToValue(const ValueStr: string;
  TypeKind: TTypeKind; TypeInfo: PTypeInfo): TValue;
begin
  case TypeKind of
    tkInteger, tkInt64:
      Result := StrToIntDef(ValueStr, 0);
    tkFloat:
      begin
        if TypeInfo = System.TypeInfo(TDateTime) then
          Result := StrToDateTimeDef(ValueStr, 0)
        else if TypeInfo = System.TypeInfo(TDate) then
          Result := StrToDateDef(ValueStr, 0)
        else if TypeInfo = System.TypeInfo(TTime) then
          Result := StrToTimeDef(ValueStr, 0)
        else
          Result := StrToFloatDef(ValueStr, 0.0);
      end;
    tkEnumeration:
      begin
        if TypeInfo = System.TypeInfo(Boolean) then
          Result := SameText(ValueStr, 'True') or SameText(ValueStr, '1') or
                   SameText(ValueStr, 'Yes') or SameText(ValueStr, '是')
        else
          Result := TValue.FromOrdinal(TypeInfo, GetEnumValue(TypeInfo, ValueStr));
      end;
    tkString, tkLString, tkWString, tkUString:
      Result := ValueStr;
    tkSet:
      begin
        Result := TValue.FromOrdinal(TypeInfo, StringToSet(TypeInfo, ValueStr));
      end;
  else
    Result := ValueStr;
  end;
end;

class function TObjectConfigSerializer.ValueToString(Value: TValue): string;
begin
  case Value.Kind of
    tkInteger, tkInt64:
      Result := IntToStr(Value.AsInteger);
    tkFloat:
      begin
        if Value.TypeInfo = System.TypeInfo(TDateTime) then
          Result := DateTimeToStr(Value.AsExtended)
        else if Value.TypeInfo = System.TypeInfo(TDate) then
          Result := DateToStr(Value.AsExtended)
        else if Value.TypeInfo = System.TypeInfo(TTime) then
          Result := TimeToStr(Value.AsExtended)
        else
          Result := FloatToStr(Value.AsExtended);
      end;
    tkEnumeration:
      begin
        if Value.TypeInfo = System.TypeInfo(Boolean) then
          Result := BoolToStr(Value.AsBoolean, True)
        else
          Result := GetEnumName(Value.TypeInfo, Value.AsOrdinal);
      end;
    tkSet:
      begin
        Result := SetToString(Value.TypeInfo, Value.AsOrdinal, True);
      end;
    tkString, tkLString, tkWString, tkUString:
      Result := Value.AsString;
  else
    Result := Value.ToString;
  end;
end;

class function TObjectConfigSerializer.ValidatePropertyValue(const ValueStr: string;
  TypeKind: TTypeKind; TypeInfo: PTypeInfo; out ErrorMsg: string): Boolean;
begin
  Result := True;
  ErrorMsg := '';

  try
    case TypeKind of
      tkInteger, tkInt64:
        StrToInt(ValueStr);
      tkFloat:
        begin
          if TypeInfo = System.TypeInfo(TDateTime) then
            StrToDateTime(ValueStr)
          else if TypeInfo = System.TypeInfo(TDate) then
            StrToDate(ValueStr)
          else if TypeInfo = System.TypeInfo(TTime) then
            StrToTime(ValueStr)
          else
            StrToFloat(ValueStr);
        end;
      tkEnumeration:
        begin
          if TypeInfo = System.TypeInfo(Boolean) then
          begin
            if not (SameText(ValueStr, 'True') or SameText(ValueStr, 'False') or
                    SameText(ValueStr, '1') or SameText(ValueStr, '0') or
                    SameText(ValueStr, 'Yes') or SameText(ValueStr, 'No') or
                    SameText(ValueStr, '是') or SameText(ValueStr, '否')) then
              raise EConvertError.Create('无效的布尔值');
          end
          else
            GetEnumValue(TypeInfo, ValueStr);
        end;
    end;
  except
    on E: Exception do
    begin
      Result := False;
      ErrorMsg := E.Message;
    end;
  end;
end;

class procedure TObjectConfigSerializer.ApplyConfigData(Obj: TBaseConfig);
begin
  ApplyConfigData(Obj.MyData, Obj);

  if Obj.FFileName <> '' then
  begin
    //保存
    TObjectConfigSerializer.SaveToFile(Obj, Obj.FFileName);
  end;
end;

class function TObjectConfigSerializer.CreateConfigData(Obj: TObject): TConfigData;
var
  Context: TRttiContext;
  AType: TRttiType;
  Prop: TRttiProperty;
  Value: TValue;
  PropInfo: TConfigPropertyInfo;
begin
  Result := TConfigData.Create(Obj.ClassType);

  Context := TRttiContext.Create;
  try
    AType := Context.GetType(Obj.ClassType);

    for Prop in AType.GetProperties do
    begin
      if Prop.Visibility = mvPublished then
      begin
        Value := Prop.GetValue(Obj);

        PropInfo.Name := Prop.Name;
        PropInfo.Value := ValueToString(Value);
        PropInfo.Description := GetPropertyDescription(Obj, Prop.Name);
        PropInfo.DataType := GetPropertyTypeName(Prop.PropertyType.TypeKind, Prop.PropertyType.Handle);
        PropInfo.TypeKind := Prop.PropertyType.TypeKind;
        PropInfo.TypeInfo := Prop.PropertyType.Handle;
        PropInfo.IsValid := True;
        PropInfo.ValidationError := '';

        Result.Properties.Add(PropInfo);
      end;
    end;
  finally
    Context.Free;
  end;
end;

class procedure TObjectConfigSerializer.ApplyConfigData(ConfigData: TConfigData; Obj: TObject);
var
  Context: TRttiContext;
  AType: TRttiType;
  Prop: TRttiProperty;
  I: Integer;
  PropInfo: TConfigPropertyInfo;
  ErrorMsg: string;
begin
  Context := TRttiContext.Create;
  try
    AType := Context.GetType(Obj.ClassType);

    for I := 0 to ConfigData.Properties.Count - 1 do
    begin
      PropInfo := ConfigData.Properties[I];

      // 查找对应的属性
      Prop := AType.GetProperty(PropInfo.Name);
      if Assigned(Prop) and (Prop.Visibility = mvPublished) then
      begin
        // 验证属性值
        if ValidatePropertyValue(PropInfo.Value, Prop.PropertyType.TypeKind,
           Prop.PropertyType.Handle, ErrorMsg) then
        begin
          // 将字符串转换为适当类型的值并设置属性
          Prop.SetValue(Obj, StringToValue(PropInfo.Value,
            Prop.PropertyType.TypeKind, Prop.PropertyType.Handle));
        end
        else
        begin
          raise EConvertError.CreateFmt('属性 "%s" 的值 "%s" 无效: %s',
            [PropInfo.Name, PropInfo.Value, ErrorMsg]);
        end;
      end;
    end;
  finally
    Context.Free;
  end;

  // 调用 AfterLoad 钩子
  if Obj is TBaseConfig then
    TBaseConfig(Obj).AfterLoad;
end;

class procedure TObjectConfigSerializer.ObjectToStrings(Obj: TObject; Strings: TStrings);
var
  ConfigData: TConfigData;
  I: Integer;
  PropInfo: TConfigPropertyInfo;
begin
  Strings.BeginUpdate;
  try
    Strings.Clear;

    ConfigData := CreateConfigData(Obj);
    try
      for I := 0 to ConfigData.Properties.Count - 1 do
      begin
        PropInfo := ConfigData.Properties[I];
        Strings.Values[PropInfo.Name] := PropInfo.Value;
      end;
    finally
      ConfigData.Free;
    end;
  finally
    Strings.EndUpdate;
  end;
end;

class procedure TObjectConfigSerializer.StringsToObject(Strings: TStrings; Obj: TObject);
var
  ConfigData: TConfigData;
  I: Integer;
  Key, Value: string;
begin
  ConfigData := CreateConfigData(Obj);
  try
    for I := 0 to Strings.Count - 1 do
    begin
      Key := Strings.Names[I];
      Value := Strings.ValueFromIndex[I];

      if Key <> '' then
        ConfigData.UpdateProperty(Key, Value);
    end;

    ApplyConfigData(ConfigData, Obj);
  finally
    ConfigData.Free;
  end;
end;

class procedure TObjectConfigSerializer.SaveToFile(Obj: TObject; const FileName: string);
var
  Stream: TFileStream;
begin
  Stream := TFileStream.Create(FileName, fmCreate);
  try
    SaveToStream(Obj, Stream);
  finally
    Stream.Free;
  end;
end;

class procedure TObjectConfigSerializer.LoadFromFile(Obj: TObject; const FileName: string);
var
  Stream: TFileStream;
begin
  if not FileExists(FileName) then
    Exit;

  Stream := TFileStream.Create(FileName, fmOpenRead or fmShareDenyWrite);
  try
    LoadFromStream(Obj, Stream);
  finally
    Stream.Free;
  end;

  if (Obj is TBaseConfig) then
  begin
    TBaseConfig(Obj).FFileName := FileName;
  end;
end;

class procedure TObjectConfigSerializer.SaveToFile(Obj: TBaseConfig);
begin
  if Obj.FFileName = '' then Exit;

  TObjectConfigSerializer.SaveToFile(Obj, Obj.FFileName);
end;

class procedure TObjectConfigSerializer.SaveToStream(Obj: TObject; Stream: TStream);
var
  Strings: TStringList;
  Context: TRttiContext;
  AType: TRttiType;
  Prop: TRttiProperty;
  Value: TValue;
  Description: string;
begin
  Strings := TStringList.Create;
  try
    // 添加文件头
    Strings.Add('# 应用程序配置文件');
    Strings.Add('# 生成时间: ' + FormatDateTime('yyyy/mm/dd hh:nn:ss', Now));
    Strings.Add('');

    // 调用 BeforeSave 钩子
    if Obj is TBaseConfig then
      TBaseConfig(Obj).BeforeSave;

    // 使用 RTTI 遍历所有 published 属性
    Context := TRttiContext.Create;
    try
      AType := Context.GetType(Obj.ClassType);

      for Prop in AType.GetProperties do
      begin
        if Prop.Visibility = mvPublished then
        begin
          // 获取属性描述
          Description := GetPropertyDescription(Obj, Prop.Name);
          if Description <> '' then
            Strings.Add('# ' + Description);

          // 获取属性值并转换为字符串
          Value := Prop.GetValue(Obj);
          Strings.Add(Prop.Name + '=' + ValueToString(Value));
          Strings.Add('');
        end;
      end;
    finally
      Context.Free;
    end;

    Strings.SaveToStream(Stream);
  finally
    Strings.Free;
  end;
end;

class procedure TObjectConfigSerializer.LoadFromStream(Obj: TObject; Stream: TStream);
var
  Strings: TStringList;
  Context: TRttiContext;
  AType: TRttiType;
  Prop: TRttiProperty;
  I: Integer;
  Line, Key, ValueStr: string;
  PosEqual: Integer;
begin
  Strings := TStringList.Create;
  try
    Strings.LoadFromStream(Stream);

    Context := TRttiContext.Create;
    try
      AType := Context.GetType(Obj.ClassType);

      for I := 0 to Strings.Count - 1 do
      begin
        Line := Trim(Strings[I]);

        // 跳过空行和注释
        if (Line = '') or (Line[1] = '#') then
          Continue;

        // 解析 Key=Value
        PosEqual := Pos('=', Line);
        if PosEqual > 0 then
        begin
          Key := Trim(Copy(Line, 1, PosEqual - 1));
          ValueStr := Trim(Copy(Line, PosEqual + 1, MaxInt));

          // 查找对应的属性
          Prop := AType.GetProperty(Key);
          if Assigned(Prop) and (Prop.Visibility = mvPublished) then
          begin
            // 将字符串转换为适当类型的值并设置属性
            Prop.SetValue(Obj, StringToValue(ValueStr, Prop.PropertyType.TypeKind, Prop.PropertyType.Handle));
          end;
        end;
      end;
    finally
      Context.Free;
    end;

    // 调用 AfterLoad 钩子
    if Obj is TBaseConfig then
    begin
      TBaseConfig(Obj).AfterLoad;

      if Assigned(TBaseConfig(Obj).FData) then
      begin
        //刷新数据对象
        TBaseConfig(Obj).FData.Free;
        TBaseConfig(Obj).FData := TObjectConfigSerializer.CreateConfigData(TBaseConfig(Obj));
      end;
    end;
  finally
    Strings.Free;
  end;
end;

end.

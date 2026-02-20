# Markdown Editor

## 版权声明：
我自己的代码，你可以随便拿去使用，随便修改。
里面引用的别人的开源代码，请遵守别人的开源协议。

## Markdown 解析器
把 Markdown 文档解析为 HTML 的代码，来自：https://github.com/EtheaDev/MarkdownProcessor
本代码仓库里面没有包含这部分代码，请自己去上述地址下载。

## Markdown 文档的预览显示
1. 显示采用 TEdgeBrowser；
2. HTML 内容采用内置的 WebServer + WebBroker 来输出给 WebBrowser；
3. 模板页面：Template.html;
4. CSS 文件：都在 "发布"文件夹下。程序执行时，WebBroker 需要将 CSS 文件加载后发送给浏览器。
5. CSS 文件和模板文件放到程序 EXE 文件相同目录下。决定这个文件夹的代码是：
~~~
function TWebModule1.GetTemplatePath: string;
begin
  Result := ExtractFilePath(ParamStr(0));
end;
~~~
所以，你可以修改这个代码来决定你想把 CSS 文件和模板文件放到哪个文件夹底下。

## 配置文件
配置文件采用 Name=Value 的文本文件，方便阅读和编辑；
把配置参数加载或者写入配置文件的代码：ObjectConfigSerializer.pas 和 UJaMarkdownConfig.pas

## 数据库
数据库用于记录经常打开的文件夹和最近打开的文件。
数据库采用 SQLite。这里没有发布 SQLite 的数据库文件。在 Delphi IDE 里面打开本项目，查看 DmJaMarkdown 这个 DataModule，打开里面的几个 FdQuery 的字段编辑器，可以查看到相关字段。因此，你可以用任何数据库，根据 FdQuery 里面的相关字段的名称，大小，类型，自己在数据库里面创建字段，然后修改 FDConnection1 的设置，去连接你的数据库就好了。

## 使用
1. 需要配置好本地服务器的端口，比如 8080，如果8080被占用，可以改为其它端口；
2. 一个 .md 文件的图片，保存到该文件相同目录下。预览时，WebServer 代码知道去哪里找这份文件。当然这个逻辑可以根据你自己的需要修改。
3. 支持 VCL Style；
4. 支持拖拽 .md 文件到本程序。但是，不能拖放到预览界面的 TEdgeBrowser 上面。
5. 如果关联 .md 文件到本程序，在资源管理器里面，双击 .md 文件，自动打开本程序并加载和显示它。本程序没有建立关联的代码，使用者可以在 Windows 资源管理器里面手动创建 .md 文件和本程序的关联。

## 解释这个程序的文章：
1. [显示 Markdown 的文件](https://blog.csdn.net/pcplayer/article/details/154800268)
2. [Markdown 的预览和编辑](https://blog.csdn.net/pcplayer/article/details/154841491)

## Summary
This is a Markdown editor and viewer, by Delphi code, based on https://github.com/EtheaDev/MarkdownProcessor

It is a Delphi VCL program.
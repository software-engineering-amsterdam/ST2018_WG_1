
<!DOCTYPE html>
<html class="scripts-not-loaded" dir="ltr"   lang="en-GB">
<head>
  <meta charset="utf-8">
  <title>  Lecture6.hs: Software Specification, Verification and Testing
</title>
  <!--[if lte IE 9]> <meta http-equiv=refresh content="0; URL=/ie-9-is-not-supported.html" /> <![endif]-->
  <link rel="preload" href="https://du11hjcvx0uqb.cloudfront.net/dist/fonts/lato/latin/LatoLatin-Regular-3cd3657802.woff2" as="font" type="font/woff2" crossorigin>
  <link rel="shortcut icon" type="image/x-icon" href="https://du11hjcvx0uqb.cloudfront.net/dist/images/favicon-e10d657a73.ico" />
  <link rel="apple-touch-icon" href="https://instructure-uploads-eu.s3-eu-west-1.amazonaws.com/account_103920000000000001/attachments/7/Homescreen%20Icon%20UvA%20180x180%20-%20v2.png" />
  
  <link rel="stylesheet" media="all" href="https://du11hjcvx0uqb.cloudfront.net/dist/brandable_css/2d7a61da268b1cae83a46f7a7bd80413/variables-750d72b9d3e5d522f965bf904110c132.css" />
  <link rel="stylesheet" media="all" href="https://du11hjcvx0uqb.cloudfront.net/dist/brandable_css/new_styles_normal_contrast/bundles/common-caecc44ef5.css" />
  <script>
//<![CDATA[

!function(){
  function get(u){document.write('<scr'+'ipt src="'+ u +'"></sc'+'ript>')}
  var o,s,v;
  if (!(window.Promise && Object.assign && Object.values && [].find && [].includes && (o={},s=Symbol(),v={},o[s]=v,o[s]===v) && (function f(){}).bind().name==='bound f')) {
    get("https://du11hjcvx0uqb.cloudfront.net/dist/ie11-polyfill-9f640d24ea.js");
  }
  window.fetch || get("https://cdnjs.cloudflare.com/ajax/libs/fetch/2.0.4/fetch.min.js");
}();
      
//]]>
</script>
  <script src="https://du11hjcvx0uqb.cloudfront.net/dist/lato-fontfaceobserver-ed903d58c4.js" async="async"></script>
  
  <meta name="apple-itunes-app" content="app-id=480883488" />
<link rel="manifest" href="/web-app-manifest/manifest.json" />
  <meta name="viewport" content="width=device-width, initial-scale=1">
  <meta name="theme-color" content="#bc0031">
  
  
  
  <script>
    function _earlyClick(e){
      var c = e.target
      while (c && c.ownerDocument) {
        if (c.getAttribute('href') == '#' || c.getAttribute('data-method')) {
          e.preventDefault()
          (_earlyClick.clicks = _earlyClick.clicks || []).push(c)
          break
        }
        c = c.parentNode
      }
    }
    document.addEventListener('click', _earlyClick)
  </script>
  <script src="https://du11hjcvx0uqb.cloudfront.net/dist/brandable_css/2d7a61da268b1cae83a46f7a7bd80413/variables-750d72b9d3e5d522f965bf904110c132.js" defer="defer"></script>
  <script src="https://du11hjcvx0uqb.cloudfront.net/dist/webpack-production/vendor.bundle-c5ffeb745c.js" defer="defer"></script>
<script src="https://du11hjcvx0uqb.cloudfront.net/dist/timezone/Europe/Amsterdam-59b781cbeb.js" defer="defer"></script>
<script src="https://du11hjcvx0uqb.cloudfront.net/dist/timezone/en_GB-31745e04c9.js" defer="defer"></script>
<script src="https://du11hjcvx0uqb.cloudfront.net/dist/webpack-production/moment/locale/en-gb.bundle-1044471520.js" defer="defer"></script>
<script src="https://du11hjcvx0uqb.cloudfront.net/dist/webpack-production/appBootstrap.bundle-f573a5fc65.js" defer="defer"></script>
<script src="https://du11hjcvx0uqb.cloudfront.net/dist/webpack-production/common.bundle-81f29f4105.js" defer="defer"></script>
<script src="https://du11hjcvx0uqb.cloudfront.net/dist/webpack-production/module_sequence_footer.bundle-be2cf927c9.js" defer="defer"></script>
<script src="https://du11hjcvx0uqb.cloudfront.net/dist/webpack-production/file_show.bundle-d4c50f2564.js" defer="defer"></script>
</head>

<body class="with-left-side course-menu-expanded files primary-nav-expanded context-course_2756 lato-font-not-loaded-yet">

<noscript>
  <div role="alert" class="ic-flash-static ic-flash-error">
    <div class="ic-flash__icon" aria-hidden="true">
      <i class="icon-warning"></i>
    </div>
    <h1> To access this site, you must enable JavaScript.</h1>
  </div>
</noscript>





<ul id="flash_message_holder"></ul>
<div id="flash_screenreader_holder"></div>

<div id="application" class="ic-app">
  
  <header id="header" class="ic-app-header no-print ">
    <a href="#content" id="skip_navigation_link">Skip to content</a>
      <div role="region" class="ic-app-header__main-navigation" aria-label="Global navigation">
        <div class="ic-app-header__logomark-container">
          <a href="https://canvas.uva.nl/" class="ic-app-header__logomark">
            <span class="screenreader-only">Dashboard</span>
          </a>
        </div>
        <ul id="menu" class="ic-app-header__menu-list">
            <li class="menu-item ic-app-header__menu-list-item ">
              <a id="global_nav_profile_link" href="/profile" class="ic-app-header__menu-list-link">
                <div class="menu-item-icon-container" aria-hidden="true">
                  <div class="ic-avatar ">
                    <img src="https://canvas.uva.nl/images/messages/avatar-50.png" alt="Remy van der Vegt" />
                  </div>
                </div>
                <div class="menu-item__text">
                  Account
                </div>
              </a>
            </li>
          <li class="ic-app-header__menu-list-item ">
            <a id="global_nav_dashboard_link" href="https://canvas.uva.nl/" class="ic-app-header__menu-list-link">
              <div class="menu-item-icon-container" aria-hidden="true">
                  <svg xmlns="http://www.w3.org/2000/svg" class="ic-icon-svg ic-icon-svg--dashboard" version="1.1" x="0" y="0" viewBox="0 0 280 200" enable-background="new 0 0 280 200" xml:space="preserve"><path d="M273.09,180.75H197.47V164.47h62.62A122.16,122.16,0,1,0,17.85,142a124,124,0,0,0,2,22.51H90.18v16.29H6.89l-1.5-6.22A138.51,138.51,0,0,1,1.57,142C1.57,65.64,63.67,3.53,140,3.53S278.43,65.64,278.43,142a137.67,137.67,0,0,1-3.84,32.57ZM66.49,87.63,50.24,71.38,61.75,59.86,78,76.12Zm147,0L202,76.12l16.25-16.25,11.51,11.51ZM131.85,53.82v-23h16.29v23Zm15.63,142.3a31.71,31.71,0,0,1-28-16.81c-6.4-12.08-15.73-72.29-17.54-84.25a8.15,8.15,0,0,1,13.58-7.2c8.88,8.21,53.48,49.72,59.88,61.81a31.61,31.61,0,0,1-27.9,46.45ZM121.81,116.2c4.17,24.56,9.23,50.21,12,55.49A15.35,15.35,0,1,0,161,157.3C158.18,152,139.79,133.44,121.81,116.2Z" /></svg>

              </div>
              <div class="menu-item__text">Dashboard</div>
            </a>
          </li>
          <li class="menu-item ic-app-header__menu-list-item ic-app-header__menu-list-item--active">
            <a id="global_nav_courses_link" href="/courses" class="ic-app-header__menu-list-link">
              <div class="menu-item-icon-container" aria-hidden="true">
                <svg xmlns="http://www.w3.org/2000/svg" class="ic-icon-svg ic-icon-svg--courses" version="1.1" x="0" y="0" viewBox="0 0 280 259" enable-background="new 0 0 280 259" xml:space="preserve"><path d="M73.31,198c-11.93,0-22.22,8-24,18.73a26.67,26.67,0,0,0-.3,3.63v.3a22,22,0,0,0,5.44,14.65,22.47,22.47,0,0,0,17.22,8H200V228.19h-134V213.08H200V198Zm21-105.74h90.64V62H94.3ZM79.19,107.34V46.92H200v60.42Zm7.55,30.21V122.45H192.49v15.11ZM71.65,16.71A22.72,22.72,0,0,0,49,39.36V190.88a41.12,41.12,0,0,1,24.32-8h157V16.71ZM33.88,39.36A37.78,37.78,0,0,1,71.65,1.6H245.36V198H215.15v45.32h22.66V258.4H71.65a37.85,37.85,0,0,1-37.76-37.76Z"/></svg>

              </div>
              <div class="menu-item__text">
                Courses
              </div>
            </a>
          </li>
            <li class="menu-item ic-app-header__menu-list-item ">
              <a id="global_nav_groups_link" href="/groups" class="ic-app-header__menu-list-link">
                <div class="menu-item-icon-container" aria-hidden="true">
                  <svg xmlns="http://www.w3.org/2000/svg" class="ic-icon-svg ic-icon-svg--groups" viewBox="0 0 200 135"><path d="M134.5 129.4c0-1.1 0-19.8-6.2-31.1-4.5-8.5-16.4-12.4-35-19.2-1.7-.6-3.4-1.1-5.1-1.7v-8.5c5.6-5.1 8.5-12.4 8.5-20.3V29.4C96.6 13 83.6 0 67.2 0S37.9 13 37.9 29.4v19.2c0 7.3 3.4 14.7 8.5 20.3v8.5c-1.7.6-3.4 1.1-5.1 1.7-18.6 6.2-30.5 10.7-35 19.2C0 109.6 0 128.8 0 129.4c0 3.4 2.3 5.6 5.6 5.6h123.7c3.5 0 5.7-2.3 5.2-5.6zm-123.2-5.7c.6-5.6 1.7-14.7 3.4-19.8C17 98.8 30 94.3 43.5 89.8c2.8-1.1 5.6-2.3 9-3.4 2.3-.6 4-2.8 4-5.1V66.7c0-1.7-.6-3.4-1.7-4.5-4-3.4-6.2-8.5-6.2-13.6V29.4c0-10.2 7.9-18.1 18.1-18.1s18.1 7.9 18.1 18.1v19.2c0 5.1-2.3 10.2-6.2 13.6-1.1 1.1-1.7 2.8-1.7 4.5v14.7c0 2.3 1.7 4.5 4 5.1 2.8 1.1 6.2 2.3 9 3.4 13.6 5.1 26.6 9.6 28.8 14.1 2.8 5.1 4 13.6 4.5 19.8H11.3zM196 79.1c-2.8-6.2-11.3-9.6-22.6-13.6l-1.7-.6v-3.4c4.5-4 6.8-9.6 6.8-15.8V35c0-12.4-9.6-22-22-22s-22 10.2-22 22v10.7c0 6.2 2.3 11.9 6.8 15.8V65l-1.7.6c-7.3 2.8-13 4.5-16.9 7.3-1.7 1.1-2.3 2.8-2.3 5.1.6 1.7 1.7 3.4 3.4 4.5 7.9 4 12.4 7.3 14.1 10.7 2.3 4.5 4 10.2 5.1 18.1.6 2.3 2.8 4.5 5.6 4.5h45.8c3.4 0 5.6-2.8 5.6-5.1 0-3.9 0-24.3-4-31.6zm-42.9 25.4c-1.1-6.8-2.8-12.4-5.1-16.9-1.7-4-5.1-6.8-9.6-10.2 1.7-1.1 3.4-1.7 5.1-2.3l5.6-2.3c1.7-.6 3.4-2.8 3.4-5.1v-9.6c0-1.7-.6-3.4-2.3-4.5-2.8-1.7-4.5-5.1-4.5-8.5V34.5c0-6.2 4.5-10.7 10.7-10.7s10.7 5.1 10.7 10.7v10.7c0 3.4-1.7 6.2-4.5 8.5-1.1 1.1-2.3 2.8-2.3 4.5v10.2c0 2.3 1.1 4.5 3.4 5.1l5.6 2.3c6.8 2.3 15.3 5.6 16.4 7.9 1.7 2.8 2.8 12.4 2.8 20.9h-35.4z"/></svg>

                </div>
                <div class="menu-item__text">
                  Groups
                </div>
              </a>
            </li>
          <li class="menu-item ic-app-header__menu-list-item ">
            <a id="global_nav_calendar_link" href="/calendar" class="ic-app-header__menu-list-link">
              <div class="menu-item-icon-container" aria-hidden="true">
                <svg xmlns="http://www.w3.org/2000/svg" class="ic-icon-svg ic-icon-svg--calendar" version="1.1" x="0" y="0" viewBox="0 0 280 280" enable-background="new 0 0 280 280" xml:space="preserve"><path d="M197.07,213.38h16.31V197.07H197.07Zm-16.31,16.31V180.76h48.92v48.92Zm-48.92-16.31h16.31V197.07H131.85Zm-16.31,16.31V180.76h48.92v48.92ZM66.62,213.38H82.93V197.07H66.62ZM50.32,229.68V180.76H99.24v48.92Zm146.75-81.53h16.31V131.85H197.07Zm-16.31,16.31V115.54h48.92v48.92Zm-48.92-16.31h16.31V131.85H131.85Zm-16.31,16.31V115.54h48.92v48.92ZM66.62,148.15H82.93V131.85H66.62ZM50.32,164.46V115.54H99.24v48.92ZM34,262.29H246V82.93H34ZM246,66.62V42.16A8.17,8.17,0,0,0,237.84,34H213.38v8.15a8.15,8.15,0,1,1-16.31,0V34H82.93v8.15a8.15,8.15,0,0,1-16.31,0V34H42.16A8.17,8.17,0,0,0,34,42.16V66.62Zm-8.15-48.92a24.49,24.49,0,0,1,24.46,24.46V278.6H17.71V42.16A24.49,24.49,0,0,1,42.16,17.71H66.62V9.55a8.15,8.15,0,0,1,16.31,0v8.15H197.07V9.55a8.15,8.15,0,1,1,16.31,0v8.15Z"/></svg>

              </div>
              <div class="menu-item__text">
                Calendar
              </div>
            </a>
          </li>
          <li class="menu-item ic-app-header__menu-list-item ">
            <a id="global_nav_conversations_link" href="/conversations" class="ic-app-header__menu-list-link">
              <div class="menu-item-icon-container" aria-hidden="true">
                <svg xmlns="http://www.w3.org/2000/svg" class="ic-icon-svg ic-icon-svg--inbox" version="1.1" x="0" y="0" viewBox="0 0 280 280" enable-background="new 0 0 280 280" xml:space="preserve"><path d="M91.72,120.75h96.56V104.65H91.72Zm0,48.28h80.47V152.94H91.72Zm0-96.56h80.47V56.37H91.72Zm160.94,34.88H228.52V10.78h-177v96.56H27.34A24.17,24.17,0,0,0,3.2,131.48V244.14a24.17,24.17,0,0,0,24.14,24.14H252.66a24.17,24.17,0,0,0,24.14-24.14V131.48A24.17,24.17,0,0,0,252.66,107.34Zm0,16.09a8.06,8.06,0,0,1,8,8v51.77l-32.19,19.31V123.44ZM67.58,203.91v-177H212.42v177ZM27.34,123.44H51.48v79.13L19.29,183.26V131.48A8.06,8.06,0,0,1,27.34,123.44ZM252.66,252.19H27.34a8.06,8.06,0,0,1-8-8V202l30,18H230.75l30-18v42.12A8.06,8.06,0,0,1,252.66,252.19Z"/></svg>

                <span class="menu-item__badge" style="display: none">0</span>
              </div>
              <div class="menu-item__text">
                Inbox
              </div>
            </a>
          </li>
            


          <li class="ic-app-header__menu-list-item">
           <a id="global_nav_help_link" class="ic-app-header__menu-list-link" data-track-category="help system" data-track-label="help button" href="http://help.instructure.com/">
              <div class="menu-item-icon-container" role="presentation">
                <svg xmlns="http://www.w3.org/2000/svg" class="ic-icon-svg menu-item__icon svg-icon-help" version="1.1" x="0" y="0" viewBox="0 0 200 200" enable-background="new 0 0 200 200" xml:space="preserve" fill="currentColor"><path d="M100,127.88A11.15,11.15,0,1,0,111.16,139,11.16,11.16,0,0,0,100,127.88Zm8.82-88.08a33.19,33.19,0,0,1,23.5,23.5,33.54,33.54,0,0,1-24,41.23,3.4,3.4,0,0,0-2.74,3.15v9.06H94.42v-9.06a14.57,14.57,0,0,1,11.13-14,22.43,22.43,0,0,0,13.66-10.27,22.73,22.73,0,0,0,2.31-17.37A21.92,21.92,0,0,0,106,50.59a22.67,22.67,0,0,0-19.68,3.88,22.18,22.18,0,0,0-8.65,17.64H66.54a33.25,33.25,0,0,1,13-26.47A33.72,33.72,0,0,1,108.82,39.8ZM100,5.2A94.8,94.8,0,1,0,194.8,100,94.91,94.91,0,0,0,100,5.2m0,178.45A83.65,83.65,0,1,1,183.65,100,83.73,83.73,0,0,1,100,183.65" transform="translate(-5.2 -5.2)"/></svg>

              </div>
              <div class="menu-item__text">
                Help
              </div>
</a>          </li>
        </ul>
      </div>
      <div class="ic-app-header__secondary-navigation">
        <ul class="ic-app-header__menu-list">
          <li class="menu-item ic-app-header__menu-list-item">
            <button
              id="primaryNavToggle"
              class="ic-app-header__menu-list-link ic-app-header__menu-list-link--nav-toggle"
              aria-label="Minimise global navigation"
              title="Minimise global navigation"
            >
              <div class="menu-item-icon-container" aria-hidden="true">
                <svg xmlns="http://www.w3.org/2000/svg" class="ic-icon-svg ic-icon-svg--navtoggle" version="1.1" x="0" y="0" width="40" height="32" viewBox="0 0 40 32" xml:space="preserve">
  <path d="M39.5,30.28V2.48H37.18v27.8Zm-4.93-13.9L22.17,4,20.53,5.61l9.61,9.61H.5v2.31H30.14l-9.61,9.61,1.64,1.64Z"/>
</svg>

              </div>
            </button>
          </li>
        </ul>
      </div>
    <div id="global_nav_tray_container"></div>
  </header>


  <div id="instructure_ajax_error_box">
    <div style="text-align: right; background-color: #fff;"><a href="#" class="close_instructure_ajax_error_box_link">Close</a></div>
    <iframe id="instructure_ajax_error_result" src="about:blank" style="border: 0;" title="Error"></iframe>
  </div>

  

  <div id="wrapper" class="ic-Layout-wrapper">
      <div class="ic-app-nav-toggle-and-crumbs no-print">
          <button type="button" id="courseMenuToggle" class="Button Button--link ic-app-course-nav-toggle" aria-live="polite" aria-label="Hide courses navigation menu" title="Hide courses navigation menu">
            <i class="icon-hamburger" aria-hidden="true"></i>
          </button>
          <div class="ic-app-crumbs">
        <nav id="breadcrumbs" role="navigation" aria-label="breadcrumbs"><ul><li class="home"><a href="/"><span class="ellipsible">      <i class="icon-home"
         title="My dashboard">
        <span class="screenreader-only">My dashboard</span>
      </i>
</span></a></li><li><a href="/courses/2756"><span class="ellipsible">5364SSVT6Y</span></a></li><li><a href="/courses/2756/files"><span class="ellipsible">Files</span></a></li><li><a href="/courses/2756/files/415028"><span class="ellipsible">Lecture6.hs</span></a></li></ul></nav>
        </div>
      </div>
    <div id="main" class="ic-Layout-columns">
        <div class="ic-Layout-watermark"></div>
        <div id="left-side"
          class="ic-app-course-menu list-view"
          style="display: block"
          >
              <span id="section-tabs-header-subtitle" class="ellipsis">2018/19 Sem. 1, per. 1</span>
            <nav role="navigation" aria-label="Courses navigation menu"><ul id="section-tabs"><li class="section"><a href="/courses/2756" title="Home" class="home">Home</a></li><li class="section"><a href="/courses/2756/grades" title="Grades" class="grades">Grades</a></li><li class="section"><a href="/courses/2756/modules" title="Modules" class="modules">Modules</a></li><li class="section"><a href="/courses/2756/announcements" title="Announcements" class="announcements">Announcements</a></li></ul></nav>
        </div>
      <div id="not_right_side" class="ic-app-main-content">
        <div id="content-wrapper" class="ic-Layout-contentWrapper">
            

          <div id="content" class="ic-Layout-contentMain" role="main">
            
  <h2>Lecture6.hs</h2>
  <div>
    <span style="font-size: 1.2em;">
      <a href="/courses/2756/files/415028/download?download_frd=1">Download Lecture6.hs</a>
    </span> (5.28 KB)
  </div>
    <div id="doc_preview" data-canvadoc_session_url="/api/v1/canvadoc_session?blob=%7B%22anonymous_instructor_annotations%22:null,%22enable_annotations%22:null,%22moderated_grading_whitelist%22:null,%22submission_id%22:null,%22user_id%22:103920000000000130,%22attachment_id%22:415028,%22type%22:%22canvadoc%22%7D&amp;hmac=6da662eeebc17c3eb3cd34c751e93a6151c2bb86" data-attachment_id="415028" data-mimetype="text/x-haskell" data-attachment_view_inline_ping_url="https://canvas.uva.nl/courses/2756/files/415028/inline_view" data-attachment_preview_processing="true"></div>

  <div id="sequence_footer" data-course-id="2756" data-asset-id="415028" data-asset-type="File"></div>


          </div>
        </div>
        <div id="right-side-wrapper" class="ic-app-main-content__secondary">
          <aside id="right-side" role="complementary">
            
          </aside>
        </div>
      </div>
    </div>
  </div>



    <div style="display:none;"><!-- Everything inside of this should always stay hidden -->
        <div id="page_view_id">091a0820-4537-425f-98c1-dfe52bedd717</div>
    </div>
  <div id='aria_alerts' class='hide-text affix' role="alert" aria-live="assertive"></div>
  <div id='StudentTray__Container'></div>
  

<script>
  INST = {"environment":"production","allowMediaComments":true,"kalturaSettings":{"domain":"eu.nv.instructuremedia.com","resource_domain":"eu.nv.instructuremedia.com","rtmp_domain":"eu.fms-prod.instructuremedia.com","partner_id":"5","subpartner_id":"0","player_ui_conf":"0","kcw_ui_conf":"0","upload_ui_conf":"0","max_file_size_bytes":534773760,"do_analytics":false,"hide_rte_button":false,"js_uploader":true},"googleAnalyticsAccount":"UA-9138420-1","logPageViews":true,"maxVisibleEditorButtons":3,"editorButtons":[{"name":"Google App","id":39,"url":"https://google-drive-lti-dub-prod.instructure.com/lti/rce-content-selection","icon_url":"https://google-drive-lti-dub-prod.instructure.com/assets/google_drive_icon-ba12a17e3e3d7f70f910b17f1b4c0500ff0298ad087a93a6c133ca623c40f0f8.png","canvas_icon_class":null,"width":700,"height":600},{"name":"Mediasite","id":76,"url":"https://webcolleges.uva.nl:443/Mediasite/LTI/","icon_url":"https://webcolleges.uva.nl:443/Mediasite/LTI/client/atom.gif","canvas_icon_class":null,"width":600,"height":600},{"name":"YouTube","id":392,"url":"https://www.edu-apps.org/lti_public_resources/?tool_id=youtube","icon_url":"https://www.edu-apps.org/assets/lti_public_resources/youtube_icon.png","canvas_icon_class":null,"width":560,"height":600}]};
  ENV = {"ASSET_HOST":"https://du11hjcvx0uqb.cloudfront.net","active_brand_config_json_url":"https://du11hjcvx0uqb.cloudfront.net/dist/brandable_css/2d7a61da268b1cae83a46f7a7bd80413/variables-750d72b9d3e5d522f965bf904110c132.json","url_to_what_gets_loaded_inside_the_tinymce_editor_css":["https://du11hjcvx0uqb.cloudfront.net/dist/brandable_css/2d7a61da268b1cae83a46f7a7bd80413/variables-750d72b9d3e5d522f965bf904110c132.css","https://du11hjcvx0uqb.cloudfront.net/dist/brandable_css/new_styles_normal_contrast/bundles/what_gets_loaded_inside_the_tinymce_editor-53dac18d10.css"],"url_for_high_contrast_tinymce_editor_css":["https://du11hjcvx0uqb.cloudfront.net/dist/brandable_css/default/variables-high_contrast-750d72b9d3e5d522f965bf904110c132.css","https://du11hjcvx0uqb.cloudfront.net/dist/brandable_css/new_styles_high_contrast/bundles/what_gets_loaded_inside_the_tinymce_editor-b4d65fe883.css"],"current_user_id":"130","current_user_roles":["user","student"],"current_user_disabled_inbox":false,"files_domain":"cluster215-files.instructure.com","DOMAIN_ROOT_ACCOUNT_ID":103920000000000001,"k12":false,"use_responsive_layout":false,"help_link_name":"Help","help_link_icon":"help","use_high_contrast":false,"LTI_LAUNCH_FRAME_ALLOWANCES":["geolocation","microphone","camera","midi","encrypted-media"],"SETTINGS":{"open_registration":false,"eportfolios_enabled":true,"collapse_global_nav":false,"show_feedback_link":true,"enable_profiles":true},"current_user":{"id":"130","display_name":"Remy van der Vegt","avatar_image_url":"https://canvas.uva.nl/images/messages/avatar-50.png","html_url":"https://canvas.uva.nl/about/130","avatar_is_fallback":true},"page_view_update_url":"/page_views/091a0820-4537-425f-98c1-dfe52bedd717?page_view_token=eyJ0eXAiOiJKV1QiLCJhbGciOiJIUzI1NiJ9.eyJpIjoiMDkxYTA4MjAtNDUzNy00MjVmLTk4YzEtZGZlNTJiZWRkNzE3IiwidSI6MTAzOTIwMDAwMDAwMDAwMTMwLCJjIjoiMjAxOC0xMC0xMVQwODozNzozOS4zNVoifQ.tRHBMoppZ4rpDgziU5gdgkufWktja3sX_4KnC3fMH88","context_asset_string":"course_2756","ping_url":"https://canvas.uva.nl/api/v1/courses/2756/ping","TIMEZONE":"Europe/Amsterdam","CONTEXT_TIMEZONE":"Europe/Amsterdam","LOCALE":"en-GB","BIGEASY_LOCALE":"en_GB","FULLCALENDAR_LOCALE":"en-gb","MOMENT_LOCALE":"en-gb","badge_counts":{"submissions":0},"notices":[]};
</script>

<script src="https://du11hjcvx0uqb.cloudfront.net/dist/webpack-production/navigation_header.bundle-017c3050c4.js" defer="defer"></script>
<script src="https://instructure-uploads-eu.s3.eu-west-1.amazonaws.com/account_103920000000000001/attachments/72202/uva_hiding_facebook_twitter_import_course_content.02.js" defer="defer"></script>

</div> <!-- #application -->
</body>
</html>
